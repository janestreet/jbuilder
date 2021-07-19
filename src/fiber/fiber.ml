open! Stdune

type 'a t = ('a -> unit) -> unit

(* This module tries to enforce the following invariants:

   - the execution context passed to a continuation is the same as the current
   one

   - the execution of a fiber always ends with [deref]

   - when an exception is raised by the user code, the exception must be
   forwarded to the execution context that was active at the time the exception
   was raised

   - when an exception is raised by the user code, then we assume that the
   current fiber didn't reach the [deref] point. As a result we have to call
   [deref] at this point on the current execution context

   Remarks:

   - most of the code assumes that errors will be caught by the caller, so when
   we do a context switch, we simply change the current execution context and
   chain to the continuation without catching errors. The current [try..with]
   will catch any raised error and forward to the current execution context. The
   only place we add a [try..with] is at the toplevel or when forking. *)

let of_thunk f k = f () k

module Execution_context : sig
  module K : sig
    (* Represent a suspended fiber *)
    type 'a t

    (* Create a continuation that captures the current execution context *)
    val create : ('a -> unit) -> 'a t

    (* Enqueue the restarting of a suspended fiber. *)
    val enqueue : 'a t -> 'a -> unit
  end

  (* Execute a function returning a fiber, passing any raised exception to the
     current execution context. This function preserve the current execution
     context. It should be called when creating forks.*)
  val apply : ('a -> 'b t) -> 'a -> 'b t

  val apply2 : ('a -> 'b -> 'c t) -> 'a -> 'b -> 'c t

  (* Add [n] references to the current execution context *)
  val add_refs : int -> unit

  (* Decrese the reference count of the current execution context *)
  val deref : unit -> unit

  (* [wait_errors f] executes [f ()] inside a new execution contexts. Returns a
     fiber that terminates when all the fiber in the sub-context have
     terminated. *)
  val wait_errors : (unit -> 'a t) -> ('a, unit) result t

  (* Set the current error handler. [on_error] is called in the current
     execution context. *)
  val set_error_handler :
    on_error:(Exn_with_backtrace.t -> unit t) -> ('a -> 'b t) -> 'a -> 'b t

  val vars : unit -> Univ_map.t

  val set_vars : Univ_map.t -> ('a -> 'b t) -> 'a -> 'b t

  val run : 'a t -> iter:(unit -> unit t) -> 'a

  val reraise_all : Exn_with_backtrace.t list -> unit
end = struct
  type t =
    { on_error : Exn_with_backtrace.t k option
          (* This handler must never raise *)
    ; vars : Univ_map.t
    ; on_release : on_release
    ; jobs : job Queue.t
    }

  and 'a on_release_exec =
    { k : ('a, unit) result k
    ; mutable ref_count : int
    }

  and on_release =
    | Do_nothing : on_release
    | Exec : _ on_release_exec -> on_release

  and 'a k =
    { run : 'a -> unit
    ; ctx : t
    }

  and job = Job : 'a k * 'a -> job

  let create () =
    { on_error = None
    ; vars = Univ_map.empty
    ; on_release = Do_nothing
    ; jobs = Queue.create ()
    }

  let current = ref (create ())

  module K = struct
    type 'a t = 'a k

    let create run = { run; ctx = !current }

    let enqueue k x =
      assert (k.ctx.jobs == !current.jobs);
      Queue.push k.ctx.jobs (Job (k, x))
  end

  let add_refs n =
    let t = !current in
    match t.on_release with
    | Do_nothing -> ()
    | Exec r -> r.ref_count <- r.ref_count + n

  let rec forward_exn_with_backtrace t exn =
    match t.on_error with
    | None -> Exn_with_backtrace.reraise exn
    | Some { ctx; run } -> (
      current := ctx;
      try run exn with
      | exn -> forward_error exn)

  and forward_error exn =
    let exn = Exn_with_backtrace.capture exn in
    forward_exn_with_backtrace !current exn

  let deref t =
    match t.on_release with
    | Do_nothing -> ()
    | Exec r -> (
      let ref_count = r.ref_count - 1 in
      r.ref_count <- ref_count;
      match ref_count with
      | 0 ->
        (* Here we might be at an arbitrary place in the code at an arbitrary
           stack depth, so it is best to enqueue the job for running the rhs of
           [wait_errors]. *)
        K.enqueue r.k (Error ())
      | _ -> assert (ref_count > 0))

  let deref () = deref !current

  let wait_errors f k =
    let t = !current in
    let on_release = { k = { ctx = t; run = k }; ref_count = 1 } in
    let child = { t with on_release = Exec on_release } in
    current := child;
    f () (fun x ->
        let ref_count = on_release.ref_count - 1 in
        on_release.ref_count <- ref_count;
        assert (ref_count = 0);
        current := t;
        k (Ok x))

  let set_error_handler ~on_error f x k =
    let t = !current in
    let run exn = on_error exn deref in
    let on_error = Some { run; ctx = t } in
    current := { t with on_error };
    f x (fun x ->
        current := t;
        k x)

  let vars () = !current.vars

  let set_vars vars f x k =
    let t = !current in
    current := { t with vars };
    f x (fun x ->
        current := t;
        k x)

  let apply f x k =
    let backup = !current in
    (try f x k with
    | exn -> forward_error exn);
    current := backup

  let apply2 f x y k =
    let backup = !current in
    (try f x y k with
    | exn -> forward_error exn);
    current := backup

  let reraise_all exns =
    let backup = !current in
    add_refs (List.length exns - 1);
    List.iter exns ~f:(forward_exn_with_backtrace backup)

  let rec run_jobs jobs =
    (* We put the [try..with] around the [while] loop to avoid setting up an
       exception handler at each iteration of the loop. *)
    try
      while not (Queue.is_empty jobs) do
        let (Job ({ run; ctx }, x)) = Queue.pop_exn jobs in
        current := ctx;
        run x
      done
    with
    | exn ->
      forward_error exn;
      run_jobs jobs

  let run fiber ~iter =
    let backup = !current in
    Exn.protect
      ~finally:(fun () -> current := backup)
      ~f:(fun () ->
        let t = create () in
        current := t;
        let result = ref None in
        apply (fun () -> fiber) () (fun x -> result := Some x);
        run_jobs t.jobs;
        let rec loop () =
          match !result with
          | Some res -> res
          | None ->
            iter () ignore;
            run_jobs t.jobs;
            (* We restore the current execution context so that [iter] always
               observe the same execution context. *)
            current := t;
            loop ()
        in
        loop ())
end

module EC = Execution_context
module K = EC.K

let return x k = k x

let never _ = ()

type ('a, 'b) fork_and_join_state =
  | Nothing_yet
  | Got_a of 'a
  | Got_b of 'b

let fork_and_join fa fb k =
  let state = ref Nothing_yet in
  EC.add_refs 1;
  EC.apply fa () (fun a ->
      match !state with
      | Nothing_yet ->
        state := Got_a a;
        EC.deref ()
      | Got_a _ -> assert false
      | Got_b b -> k (a, b));
  fb () (fun b ->
      match !state with
      | Nothing_yet ->
        state := Got_b b;
        EC.deref ()
      | Got_a a -> k (a, b)
      | Got_b _ -> assert false)

let fork_and_join_unit fa fb k =
  let state = ref Nothing_yet in
  EC.add_refs 1;
  EC.apply fa () (fun () ->
      match !state with
      | Nothing_yet ->
        state := Got_a ();
        EC.deref ()
      | Got_a _ -> assert false
      | Got_b b -> k b);
  fb () (fun b ->
      match !state with
      | Nothing_yet ->
        state := Got_b b;
        EC.deref ()
      | Got_a () -> k b
      | Got_b _ -> assert false)

module O = struct
  let ( >>> ) a b k = a (fun () -> b k)

  let ( >>= ) t f k = t (fun x -> f x k)

  let ( >>| ) t f k = t (fun x -> k (f x))

  let ( let+ ) = ( >>| )

  let ( let* ) = ( >>= )

  let ( and* ) a b = fork_and_join (fun () -> a) (fun () -> b)

  let ( and+ ) = ( and* )
end

open O

let map t ~f = t >>| f

let bind t ~f = t >>= f

let both a b =
  let* x = a in
  let* y = b in
  return (x, y)

let sequential_map l ~f =
  let rec loop l acc =
    match l with
    | [] -> return (List.rev acc)
    | x :: l ->
      let* x = f x in
      loop l (x :: acc)
  in
  loop l []

let sequential_iter l ~f =
  let rec loop l =
    match l with
    | [] -> return ()
    | x :: l ->
      let* () = f x in
      loop l
  in
  loop l

let all = sequential_map ~f:Fun.id

let list_of_option_array =
  let rec loop arr i acc =
    if i = 0 then
      acc
    else
      let i = i - 1 in
      match arr.(i) with
      | None -> assert false
      | Some x -> loop arr i (x :: acc)
  in
  fun a -> loop a (Array.length a) []

let parallel_map l ~f k =
  match l with
  | [] -> k []
  | [ x ] -> f x (fun x -> k [ x ])
  | _ ->
    let n = List.length l in
    EC.add_refs (n - 1);
    let left_over = ref n in
    let results = Array.make n None in
    List.iteri l ~f:(fun i x ->
        EC.apply f x (fun y ->
            results.(i) <- Some y;
            decr left_over;
            if !left_over = 0 then
              k (list_of_option_array results)
            else
              EC.deref ()))

let all_concurrently = parallel_map ~f:Fun.id

let[@inline always] parallel_iter_generic ~n ~iter ~f k =
  EC.add_refs (n - 1);
  let left_over = ref n in
  let k () =
    decr left_over;
    if !left_over = 0 then
      k ()
    else
      EC.deref ()
  in
  iter ~f:(fun x -> EC.apply f x k)

let parallel_iter l ~f k =
  match l with
  | [] -> k ()
  | [ x ] -> f x k
  | _ -> parallel_iter_generic ~n:(List.length l) ~iter:(List.iter l) ~f k

let all_concurrently_unit l = parallel_iter l ~f:Fun.id

let parallel_iter_set (type a s)
    (module S : Set.S with type elt = a and type t = s) t ~(f : a -> unit t) k =
  let len = S.cardinal t in
  match len with
  | 0 -> k ()
  | 1 -> f (Option.value_exn (S.min_elt t)) k
  | n -> parallel_iter_generic ~n ~iter:(S.iter t) ~f k

module Make_map_traversals (Map : Map.S) = struct
  let parallel_iter t ~f k =
    match Map.cardinal t with
    | 0 -> k ()
    | 1 ->
      let x, y = Map.choose t |> Option.value_exn in
      f x y k
    | n ->
      EC.add_refs (n - 1);
      let left_over = ref n in
      let k () =
        decr left_over;
        if !left_over = 0 then
          k ()
        else
          EC.deref ()
      in
      Map.iteri t ~f:(fun x y -> EC.apply2 f x y k)

  let parallel_map t ~f k =
    match Map.cardinal t with
    | 0 -> k Map.empty
    | 1 ->
      let x, y = Map.choose t |> Option.value_exn in
      f x y (fun y -> k (Map.singleton x y))
    | n ->
      EC.add_refs (n - 1);
      let left_over = ref n in
      let cell = ref None in
      let k (refs : _ option ref Map.t) =
        k (Map.mapi refs ~f:(fun _ r -> Option.value_exn !r))
      in
      let refs =
        Map.mapi t ~f:(fun x y ->
            let res = ref None in
            EC.apply2 f x y (fun z ->
                res := Some z;
                decr left_over;
                if !left_over = 0 then
                  Option.iter !cell ~f:k
                else
                  EC.deref ());
            res)
      in
      if !left_over = 0 then
        k refs
      else
        cell := Some refs
end
[@@inline always]

let rec repeat_while : 'a. f:('a -> 'a option t) -> init:'a -> unit t =
 fun ~f ~init ->
  let* result = f init in
  match result with
  | None -> return ()
  | Some init -> repeat_while ~f ~init

module Var = struct
  include Univ_map.Key

  let get var k = k (Univ_map.find (EC.vars ()) var)

  let get_exn var k = k (Univ_map.find_exn (EC.vars ()) var)

  let set var x f k = EC.set_vars (Univ_map.set (EC.vars ()) var x) f () k

  let unset var f k = EC.set_vars (Univ_map.remove (EC.vars ()) var) f () k

  let create () = create ~name:"var" (fun _ -> Dyn.Encoder.string "var")
end

(* This function violates the invariant that every fiber either returns a value
   or fails with one or more errors: if [on_error] does not re-raise the
   exception, then the fiber returned by [with_error_handler_internal] fails
   with 0 errors. *)
let with_error_handler_internal f ~on_error k =
  EC.set_error_handler ~on_error f () k

let with_error_handler f ~on_error k =
  EC.set_error_handler
    ~on_error:(fun (x : Exn_with_backtrace.t) ->
      map (on_error x) ~f:Nothing.unreachable_code)
    f () k

let wait_errors f k = EC.wait_errors f k

let map_reduce_errors (type a) (module M : Monoid with type t = a) ~on_error f k
    =
  let acc = ref M.empty in
  let on_error exn =
    let+ m = on_error exn in
    acc := M.combine !acc m
  in
  wait_errors
    (fun () -> with_error_handler_internal ~on_error f)
    (function
      | Ok _ as ok -> k ok
      | Error () -> k (Error !acc))

let collect_errors f =
  let module Exns = Monoid.Appendable_list (Exn_with_backtrace) in
  let+ res =
    map_reduce_errors
      (module Exns)
      f
      ~on_error:(fun e -> return (Appendable_list.singleton e))
  in
  match res with
  | Ok x -> Ok x
  | Error l -> Error (Appendable_list.to_list l)

let reraise_all = function
  | [] -> never
  | [ exn ] -> Exn_with_backtrace.reraise exn
  | exns ->
    EC.reraise_all exns;
    never

let finalize f ~finally =
  let* res1 = collect_errors f in
  let* res2 = collect_errors finally in
  let res =
    match (res1, res2) with
    | Ok x, Ok () -> Ok x
    | Error l, Ok _
    | Ok _, Error l ->
      Error l
    | Error l1, Error l2 -> Error (l1 @ l2)
  in
  match res with
  | Ok x -> return x
  | Error l -> reraise_all l

module Ivar = struct
  type 'a state =
    | Full of 'a
    | Empty of 'a K.t Queue.t

  type 'a t = { mutable state : 'a state }

  let create () = { state = Empty (Queue.create ()) }

  let fill t x k =
    match t.state with
    | Full _ -> failwith "Fiber.Ivar.fill"
    | Empty q ->
      t.state <- Full x;
      Queue.iter q ~f:(fun k -> K.enqueue k x);
      k ()

  let read t k =
    match t.state with
    | Full x -> k x
    | Empty q -> Queue.push q (K.create k)

  let peek t k =
    k
      (match t.state with
      | Full x -> Some x
      | Empty _ -> None)
end

module Mvar = struct
  type 'a t =
    { writers : ('a * unit K.t) Queue.t
    ; readers : 'a K.t Queue.t
    ; mutable value : 'a option
    }

  (* Invariant enforced on mvars. We don't actually call this function, but we
     keep it here for documentation and to help understand the
     implementation: *)
  let _invariant t =
    match t.value with
    | None -> Queue.is_empty t.writers
    | Some _ -> Queue.is_empty t.readers

  let create () =
    { value = None; writers = Queue.create (); readers = Queue.create () }

  let create_full x =
    { value = Some x; writers = Queue.create (); readers = Queue.create () }

  let read t k =
    match t.value with
    | None -> Queue.push t.readers (K.create k)
    | Some v -> (
      match Queue.pop t.writers with
      | None ->
        t.value <- None;
        k v
      | Some (v', w) ->
        t.value <- Some v';
        K.enqueue w ();
        k v)

  let write t x k =
    match t.value with
    | Some _ -> Queue.push t.writers (x, K.create k)
    | None -> (
      match Queue.pop t.readers with
      | None ->
        t.value <- Some x;
        k ()
      | Some r ->
        K.enqueue r x;
        k ())
end

module Mutex = struct
  type t =
    { mutable locked : bool
    ; mutable waiters : unit K.t Queue.t
    }

  let lock t k =
    if t.locked then
      Queue.push t.waiters (K.create k)
    else (
      t.locked <- true;
      k ()
    )

  let unlock t k =
    assert t.locked;
    match Queue.pop t.waiters with
    | None ->
      t.locked <- false;
      k ()
    | Some next ->
      K.enqueue next ();
      k ()

  let with_lock t f =
    let* () = lock t in
    finalize f ~finally:(fun () -> unlock t)

  let create () = { locked = false; waiters = Queue.create () }
end

module Throttle = struct
  type t =
    { mutable size : int
    ; mutable running : int
    ; waiting : unit Ivar.t Queue.t
    }

  let create size = { size; running = 0; waiting = Queue.create () }

  let size t = t.size

  let running t = t.running

  let rec restart t =
    if t.running >= t.size then
      return ()
    else
      match Queue.pop t.waiting with
      | None -> return ()
      | Some ivar ->
        t.running <- t.running + 1;
        let* () = Ivar.fill ivar () in
        restart t

  let resize t n =
    t.size <- n;
    restart t

  let run t ~f =
    finalize
      ~finally:(fun () ->
        t.running <- t.running - 1;
        restart t)
      (fun () ->
        if t.running < t.size then (
          t.running <- t.running + 1;
          f ()
        ) else
          let waiting = Ivar.create () in
          Queue.push t.waiting waiting;
          let* () = Ivar.read waiting in
          f ())
end

module Stream = struct
  module In = struct
    (* Invariant: once [read] has returned [None], it always returns [None] *)
    type nonrec 'a t =
      { mutable read : unit -> 'a option t
      ; mutable reading : bool
      }

    let create_unchecked read = { read; reading = false }

    let create read =
      let t = { read; reading = false } in
      let read () =
        let+ x = read () in
        if Option.is_none x then t.read <- (fun () -> return None);
        x
      in
      t.read <- read;
      t

    let lock t =
      if t.reading then Code_error.raise "Fiber.Stream.In: already reading" [];
      t.reading <- true

    let unlock t = t.reading <- false

    let read t =
      lock t;
      let+ x = t.read () in
      unlock t;
      x

    let empty () = create_unchecked (fun () -> return None)

    let concat (type a) (xs : a t list) =
      let remains = ref xs in
      let rec go () =
        match !remains with
        | [] -> return None
        | x :: xs -> (
          let* v = read x in
          match v with
          | Some v -> return (Some v)
          | None ->
            remains := xs;
            go ())
      in
      create go

    let append x y = concat [ x; y ]

    let of_list xs =
      let xs = ref xs in
      create_unchecked (fun () ->
          match !xs with
          | [] -> return None
          | x :: xs' ->
            xs := xs';
            return (Some x))

    let cons a t = concat [ of_list [ a ]; t ]

    let filter_map t ~f =
      let rec read () =
        t.read () >>= function
        | None ->
          unlock t;
          return None
        | Some x -> (
          match f x with
          | None -> read ()
          | Some y -> return (Some y))
      in
      lock t;
      create_unchecked read

    let sequential_iter t ~f =
      let rec loop t ~f =
        t.read () >>= function
        | None ->
          unlock t;
          return ()
        | Some x ->
          let* () = f x in
          loop t ~f
      in
      lock t;
      loop t ~f

    let parallel_iter t ~f k =
      let n = ref 1 in
      let k () =
        decr n;
        if !n = 0 then (
          unlock t;
          k ()
        ) else
          EC.deref ()
      in
      let rec loop t =
        t.read () (function
          | None -> k ()
          | Some x ->
            EC.add_refs 1;
            incr n;
            EC.apply f x k;
            loop t)
      in
      loop t
  end

  module Out = struct
    type nonrec 'a t =
      { mutable write : 'a option -> unit t
      ; mutable writing : bool
      }

    let lock t =
      if t.writing then Code_error.raise "Fiber.Stream.Out: already writing" [];
      t.writing <- true

    let unlock t = t.writing <- false

    let create write =
      let t = { write; writing = false } in
      let write x =
        if Option.is_none x then
          t.write <-
            (function
            | None -> return ()
            | Some _ ->
              Code_error.raise "Fiber.Stream.Out: stream output closed" []);
        write x
      in
      t.write <- write;
      t

    let write t x =
      lock t;
      let+ () = t.write x in
      unlock t

    let null () = create (fun _ -> return ())
  end

  let connect i o =
    In.lock i;
    Out.lock o;
    let rec go () =
      let* a = i.read () in
      let* () = o.write a in
      match a with
      | None ->
        In.unlock i;
        Out.unlock o;
        return ()
      | Some _ -> go ()
    in
    go ()

  let supply i o =
    In.lock i;
    Out.lock o;
    let rec go () =
      let* a = i.read () in
      match a with
      | None ->
        In.unlock i;
        Out.unlock o;
        return ()
      | Some _ ->
        let* () = o.write a in
        go ()
    in
    go ()

  let pipe () =
    let mvar = Mvar.create () in
    let i = In.create (fun () -> Mvar.read mvar) in
    let o = Out.create (fun x -> Mvar.write mvar x) in
    (i, o)
end

module Pool = struct
  type mvar =
    | Done
    | Task of (unit -> unit t)

  type status =
    | Open
    | Closed

  type t =
    { mvar : mvar Mvar.t
    ; mutable status : status
    }

  let running t k =
    match t.status with
    | Open -> k true
    | Closed -> k false

  let create () = { mvar = Mvar.create (); status = Open }

  let task t ~f k =
    match t.status with
    | Closed ->
      Code_error.raise "pool is closed. new tasks may not be submitted" []
    | Open -> Mvar.write t.mvar (Task f) k

  let stream t =
    Stream.In.create (fun () ->
        let+ next = Mvar.read t.mvar in
        match next with
        | Done -> None
        | Task task -> Some task)

  let stop t k =
    match t.status with
    | Closed -> k ()
    | Open ->
      t.status <- Closed;
      Mvar.write t.mvar Done k

  let run t = stream t |> Stream.In.parallel_iter ~f:(fun task -> task ())
end

type fill = Fill : 'a Ivar.t * 'a -> fill

let run t ~iter =
  EC.run t ~iter:(fun () ->
      let (Fill (ivar, v)) = iter () in
      Ivar.fill ivar v)
