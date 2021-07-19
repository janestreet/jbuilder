open Stdune
open Memo.Build.O
module Caml_lazy = Lazy
open Memo
open Dune_tests_common

module Scheduler = struct
  let t = Test_scheduler.create ()

  let yield () = Test_scheduler.yield t

  let run f = Test_scheduler.run t f
end

let () = init ()

let printf = Printf.printf

let () = Memo.Perf_counters.enable ()

let () = Memo.Debug.check_invariants := true

let print_perf_counters () =
  printf "%s\n" (Memo.Perf_counters.report_for_current_run ());
  Memo.Perf_counters.assert_invariants ()

let string_fn_create name =
  Memo.create name ~input:(module String) ~cutoff:String.equal

let int_fn_create name ~cutoff = Memo.create name ~input:(module Int) ~cutoff

(* to run a computation *)
let run m = Scheduler.run (Memo.Build.run m)

let run_memo f v =
  try run (Memo.exec f v) with
  | Memo.Error.E err -> raise (Memo.Error.get err)

let run_and_log_errors m =
  match Scheduler.run (Fiber.collect_errors (fun () -> Memo.Build.run m)) with
  | Ok res -> res
  | Error exns ->
    List.iter exns ~f:(fun exn ->
        Format.printf "Error: %a@." Pp.to_fmt
          (Dyn.pp (Exn_with_backtrace.to_dyn exn)))

(* the trivial dependencies are simply the identity function *)
let compdep x = Memo.Build.return (x ^ x)

(* our two dependencies are called some and another *)
let mcompdep1 = string_fn_create "some" compdep

let mcompdep2 = string_fn_create "another" compdep

(* compute the dependencies once so they are present in the global hash table *)
let () =
  ignore (run_memo mcompdep1 "a");
  ignore (run_memo mcompdep2 "a")

(* define a counter so we can track how often our computation has been run *)
let counter = ref 0

(* our computation increases the counter, adds the two dependencies, "some" and
   "another" and works by multiplying the input by two *)
let comp x =
  let+ a =
    Memo.Build.return x >>= Memo.exec mcompdep1 >>= Memo.exec mcompdep2
  in
  counter := !counter + 1;
  String.sub a ~pos:0 ~len:(String.length a |> min 3)

let mcomp = string_fn_create "test" comp

(* running it the first time should increase the counter, running it again
   should not, but should still return the same result *)
let%expect_test _ =
  Format.printf "%d@." !counter;
  print_endline (run_memo mcomp "a");
  Format.printf "%d@." !counter;
  print_endline (run_memo mcomp "a");
  Format.printf "%d@." !counter;
  [%expect {|
    0
    aaa
    1
    aaa
    1
  |}]

let print_deps memo input =
  let open Dyn.Encoder in
  Memo.For_tests.get_deps memo input
  |> option (list (pair (option string) (fun x -> x)))
  |> print_dyn

let%expect_test _ =
  print_deps mcomp "a";
  [%expect {|
    Some [ (Some "some", "a"); (Some "another", "aa") ]
  |}]

let%expect_test _ =
  (* running it on a new input should cause it to recompute the first time it is
     run *)
  print_endline (run_memo mcomp "hello");
  Format.printf "%d@." !counter;
  print_endline (run_memo mcomp "hello");
  Format.printf "%d@." !counter;
  [%expect {|
    hel
    2
    hel
    2
  |}]

let%expect_test _ =
  (* updating the first dependency should require recomputation of mcomp 7 *)
  print_endline (run_memo mcompdep1 "testtest");
  print_endline (run_memo mcomp "hello");
  Format.printf "%d@." !counter;
  print_endline (run_memo mcomp "hello");
  Format.printf "%d@." !counter;
  [%expect {|
    testtesttesttest
    hel
    2
    hel
    2
  |}]

let stack = ref []

let dump_stack v =
  let* s = get_call_stack () in
  stack := s;
  Memo.Build.return v

let mcompcycle =
  let mcompcycle = Fdecl.create Dyn.Encoder.opaque in
  let compcycle x =
    let* x = Memo.Build.return x >>= dump_stack in
    counter := !counter + 1;
    if !counter < 20 then
      (x + 1) mod 3 |> Memo.exec (Fdecl.get mcompcycle)
    else
      failwith "cycle"
  in
  let fn = int_fn_create "cycle" compcycle ~cutoff:Int.equal in
  Fdecl.set mcompcycle fn;
  fn

let%expect_test _ =
  counter := 0;
  try run_memo mcompcycle 5 |> ignore with
  | Cycle_error.E err ->
    let cycle =
      Cycle_error.get err
      |> List.filter_map ~f:(Memo.Stack_frame.as_instance_of ~of_:mcompcycle)
    in
    print (Pp.enumerate cycle ~f:(Pp.textf "%d"));
    print (Pp.textf "%d" !counter);
    !stack
    |> List.map ~f:(fun st ->
           let open Dyn.Encoder in
           pair (option string)
             (fun x -> x)
             (Stack_frame.name st, Stack_frame.input st))
    |> Dyn.Encoder.list (fun x -> x)
    |> print_dyn;
    [%expect
      {|
      - 2
      - 1
      - 0
      4
      [ (Some "cycle", 2)
      ; (Some "cycle", 1)
      ; (Some "cycle", 0)
      ; (Some "cycle", 5)
      ]
    |}]

let mfib =
  let mfib = Fdecl.create Dyn.Encoder.opaque in
  let compfib x =
    let mfib = Memo.exec (Fdecl.get mfib) in
    counter := !counter + 1;
    if x <= 1 then
      Memo.Build.return x
    else
      let* r1 = mfib (x - 1) in
      let+ r2 = mfib (x - 2) in
      r1 + r2
  in
  let fn = int_fn_create "fib" compfib ~cutoff:Int.equal in
  Fdecl.set mfib fn;
  fn

let%expect_test _ =
  Memo.Perf_counters.reset ();
  counter := 0;
  Format.printf "%d@." (run_memo mfib 2000);
  Format.printf "%d@." !counter;
  Format.printf "%d@." (run_memo mfib 1800);
  Format.printf "%d@." !counter;
  [%expect
    {|
    2406280077793834213
    2001
    3080005411477819488
    2001
  |}];
  print_perf_counters ();
  [%expect
    {|
      Memo: 2001/2001 computed/total nodes, 3998/3998 traversed/total edges
      Memo's cycle detection graph: 0/0 nodes/edges
  |}]

let make_f name = Memo.create name ~cutoff:String.equal

let id =
  let f = make_f "id" ~input:(module String) Memo.Build.return in
  Memo.exec f

module Test_lazy (Lazy : sig
  type 'a t

  val create : (unit -> 'a Memo.Build.t) -> 'a t

  val force : 'a t -> 'a Memo.Build.t
end) =
struct
  let lazy_memo =
    let f =
      Memo.create "lazy_memo"
        ~input:(module String)
        (fun s -> Memo.Build.return (Lazy.create (fun () -> id ("lazy: " ^ s))))
    in
    Memo.exec f

  let f1_def, f1 =
    let f =
      make_f "f1"
        ~input:(module String)
        (fun s ->
          let+ s = lazy_memo s >>= Lazy.force in
          "f1: " ^ s)
    in
    (f, Memo.exec f)

  let f2_def, f2 =
    let f =
      make_f "f2"
        ~input:(module String)
        (fun s ->
          let+ s = lazy_memo s >>= Lazy.force in
          "f2: " ^ s)
    in
    (f, Memo.exec f)

  let run () =
    run
      (let* x = f1 "foo" in
       let* y = f2 "foo" in
       Memo.Build.return (x, y))

  let deps () =
    let open Dyn.Encoder in
    let conv = option (list (pair (option string) (fun x -> x))) in
    pair conv conv
      (For_tests.get_deps f1_def "foo", For_tests.get_deps f2_def "foo")
end

module Builtin_lazy = Test_lazy (struct
  type 'a t = 'a Memo.Build.t Stdlib.Lazy.t

  let create = Stdlib.Lazy.from_fun

  let force = Stdlib.Lazy.force
end)

let%expect_test _ =
  Builtin_lazy.run () |> Dyn.Encoder.(pair string string) |> print_dyn;
  [%expect {|
    ("f1: lazy: foo", "f2: lazy: foo")
  |}]

let%expect_test _ =
  (* This test used to demonstrate a bug due to a bad interaction between [lazy]
     and synchronous memoized functions. The dependency on [lazy] was only
     registered by one of the dependents below, which meant we couldn't safely
     use [lazy] together with [Memo].

     Now that [Memo] doesn't support memoization of synchronous functions
     anymore, we can freely mix [lazy] and [Memo]. *)
  Builtin_lazy.deps () |> print_dyn;
  [%expect
    {|
      (Some [ (Some "lazy_memo", "foo"); (Some "id", "lazy: foo") ],
      Some [ (Some "lazy_memo", "foo"); (Some "id", "lazy: foo") ])
    |}]

module Memo_lazy = Test_lazy (struct
  include Memo.Lazy

  (* Here we hide the optional argument [cutoff] of [Memo.Lazy.create]. *)
  let create f = create f
end)

let%expect_test _ =
  Memo_lazy.run () |> Dyn.Encoder.(pair string string) |> print_dyn;
  [%expect {|
    ("f1: lazy: foo", "f2: lazy: foo")
  |}]

let%expect_test _ =
  Memo_lazy.deps () |> print_dyn;
  [%expect
    {|
    (Some [ (Some "lazy_memo", "foo"); (None, ()) ],
    Some [ (Some "lazy_memo", "foo"); (None, ()) ])
  |}]

(* Tests for depending on the current run *)

let depends_on_run =
  Memo.create "foobar"
    ~input:(module Unit)
    ~cutoff:Unit.equal
    (fun () ->
      let+ (_ : Memo.Run.t) = Memo.current_run () in
      print_endline "running foobar")

let%expect_test _ =
  run (Memo.exec depends_on_run ());
  run (Memo.exec depends_on_run ());
  print_endline "resetting memo";
  Memo.reset Invalidation.empty;
  run (Memo.exec depends_on_run ());
  [%expect {|
    running foobar
    resetting memo
    running foobar |}]

(* Tests for Memo.Cell *)

let%expect_test _ =
  let f x = Memo.Build.return ("*" ^ x) in
  let memo =
    Memo.create "for-cell" ~input:(module String) ~cutoff:String.equal f
  in
  let cell = Memo.cell memo "foobar" in
  print_endline (run (Cell.read cell));
  print_endline (run (Cell.read cell));
  [%expect {|
    *foobar
    *foobar |}]

let%expect_test "fib linked list" =
  Memo.Perf_counters.reset ();
  let module Element = struct
    type t =
      { prev_cell : (int, t) Memo.Cell.t
      ; value : int
      ; next_cell : (int, t) Memo.Cell.t
      }
  end in
  let force cell : Element.t Memo.Build.t = Memo.Cell.read cell in
  let memo_fdecl = Fdecl.create Dyn.Encoder.opaque in
  let compute_element x =
    let memo = Fdecl.get memo_fdecl in
    printf "computing %d\n" x;
    let prev_cell = Memo.cell memo (x - 1) in
    let+ value =
      if x < 1 then
        Memo.Build.return 0
      else if x = 1 then
        Memo.Build.return 1
      else
        let* x = force prev_cell
        and* y = force prev_cell in
        let+ z = force y.prev_cell in
        x.value + z.value
    in
    { Element.next_cell = Memo.cell memo (x + 1); prev_cell; value }
  in
  let memo = Memo.create "fib" ~input:(module Int) compute_element in
  Fdecl.set memo_fdecl memo;
  let fourth = run (Memo.exec memo 4) in
  printf "4th: %d\n" fourth.value;
  printf "next: %d\n" (run (force fourth.next_cell)).value;
  let seventh = run (Memo.exec memo 7) in
  printf "7th: %d\n" seventh.value;
  printf "prev: %d\n" (run (force seventh.prev_cell)).value;
  printf "prev: %d\n"
    (run (force seventh.prev_cell >>= fun x -> force x.prev_cell)).value;
  [%expect
    {|
    computing 4
    computing 3
    computing 2
    computing 1
    computing 0
    4th: 3
    computing 5
    next: 5
    computing 7
    computing 6
    7th: 13
    prev: 8
    prev: 5
  |}];
  (* Note that duplicate dependencies are not filtered out. *)
  print_deps memo 5;
  [%expect {| Some [ (Some "fib", 4); (Some "fib", 4); (Some "fib", 3) ] |}];
  print_perf_counters ();
  [%expect
    {|
    Memo: 8/8 computed/total nodes, 18/18 traversed/total edges
    Memo's cycle detection graph: 0/0 nodes/edges
  |}]

let%expect_test "previously_evaluated_cell" =
  let f x =
    printf "Evaluating %s...\n" x;
    Memo.Build.return ("[" ^ x ^ "]")
  in
  let memo =
    Memo.create "boxed" ~input:(module String) ~cutoff:String.equal f
  in
  let evaluate_and_print name =
    let cell = Memo.cell memo name in
    printf "%s = %s\n" name (run (Cell.read cell))
  in
  let print_previously_evaluated_cell name =
    match Memo.Expert.previously_evaluated_cell memo name with
    | None -> printf "previously_evaluated_cell %s = None\n" name
    | Some cell ->
      printf "previously_evaluated_cell %s = %s\n" name (run (Cell.read cell))
  in
  let invalidate_if_evaluated name =
    match Memo.Expert.previously_evaluated_cell memo name with
    | None -> Memo.Invalidation.empty
    | Some cell ->
      printf "Invalidating %s...\n" name;
      Cell.invalidate cell
  in
  print_previously_evaluated_cell "x";
  print_previously_evaluated_cell "y";
  (* Cells are initially unevaluated. *)
  [%expect
    {|
    previously_evaluated_cell x = None
    previously_evaluated_cell y = None
  |}];
  evaluate_and_print "x";
  print_previously_evaluated_cell "x";
  print_previously_evaluated_cell "y";
  (* Only x is evaluated. *)
  [%expect
    {|
    Evaluating x...
    x = [x]
    previously_evaluated_cell x = [x]
    previously_evaluated_cell y = None
  |}];
  Memo.reset
    (Memo.Invalidation.combine
       (invalidate_if_evaluated "x")
       (invalidate_if_evaluated "y"));
  (* Only x got invalidated. *)
  [%expect {|
    Invalidating x...
  |}];
  evaluate_and_print "x";
  evaluate_and_print "y";
  print_previously_evaluated_cell "x";
  print_previously_evaluated_cell "y";
  (* Both are evaluated (x is re-evaluated because it was invalidated). *)
  [%expect
    {|
    Evaluating x...
    x = [x]
    Evaluating y...
    y = [y]
    previously_evaluated_cell x = [x]
    previously_evaluated_cell y = [y]
  |}];
  Memo.reset Memo.Invalidation.empty;
  print_previously_evaluated_cell "x";
  print_previously_evaluated_cell "y";
  (* Both are still evaluated after incrementing the current run. *)
  [%expect
    {|
    previously_evaluated_cell x = [x]
    previously_evaluated_cell y = [y]
  |}];
  Memo.reset Memo.Invalidation.clear_caches;
  (* Both switch back to unevaluated after clearing all memoization caches. *)
  print_previously_evaluated_cell "x";
  print_previously_evaluated_cell "y";
  [%expect
    {|
    previously_evaluated_cell x = None
    previously_evaluated_cell y = None
    |}]

module Function = struct
  type 'a input =
    | I : int Type_eq.Id.t * int -> int input
    | S : string Type_eq.Id.t * string -> string input

  type 'a output = 'a list

  let name = "memo-poly"

  let id (type a) (x : a input) : a Type_eq.Id.t =
    match x with
    | I (id, _) -> id
    | S (id, _) -> id

  let to_dyn _ = Dyn.Opaque

  let eval (type a) (x : a input) : a output Memo.Build.t =
    match x with
    | I (_, i) ->
      let* () = Memo.Build.return () in
      printf "Evaluating %d\n" i;
      Memo.Build.return (List.init i ~f:(fun i -> i + 1))
    | S (_, s) ->
      let* () = Memo.Build.return () in
      printf "Evaluating %S\n" s;
      Memo.Build.return [ s ]

  let get (type a) (x : a input) : a =
    match x with
    | I (_, x) -> x
    | S (_, x) -> x
end

let%expect_test "Memo.Poly" =
  let module M = Memo.Poly (Function) in
  let (i1 : int Function.input) = I (Type_eq.Id.create (), 1) in
  let (i2 : int Function.input) = I (Type_eq.Id.create (), 2) in
  let (s1 : string Function.input) = S (Type_eq.Id.create (), "hi") in
  let (s2 : string Function.input) = S (Type_eq.Id.create (), "hi again") in
  let run_int i =
    let res = run (M.eval i) in
    Dyn.to_string (Dyn.List (List.map res ~f:Int.to_dyn))
  in
  let run_string s =
    let res = run (M.eval s) in
    Dyn.to_string (Dyn.List (List.map res ~f:String.to_dyn))
  in
  printf "----- First-time calls -----\n";
  printf "%d -> %s\n" (Function.get i1) (run_int i1);
  printf "%S -> %s\n" (Function.get s1) (run_string s1);
  printf "%d -> %s\n" (Function.get i2) (run_int i2);
  printf "%S -> %s\n" (Function.get s2) (run_string s2);
  printf "----- Repeated calls (memoized) -----\n";
  printf "%d -> %s\n" (Function.get i1) (run_int i1);
  printf "%S -> %s\n" (Function.get s1) (run_string s1);
  printf "%d -> %s\n" (Function.get i2) (run_int i2);
  printf "%S -> %s\n" (Function.get s2) (run_string s2);
  [%expect
    {|
    ----- First-time calls -----
    Evaluating 1
    1 -> [ 1 ]
    Evaluating "hi"
    "hi" -> [ "hi" ]
    Evaluating 2
    2 -> [ 1; 2 ]
    Evaluating "hi again"
    "hi again" -> [ "hi again" ]
    ----- Repeated calls (memoized) -----
    1 -> [ 1 ]
    "hi" -> [ "hi" ]
    2 -> [ 1; 2 ]
    "hi again" -> [ "hi again" ]
    |}]

let print_cycle_error cycle_error =
  let frames = Memo.Cycle_error.get cycle_error in
  printf "Dependency cycle detected:\n";
  List.iteri frames ~f:(fun i frame ->
      let called_by =
        match i with
        | 0 -> ""
        | _ -> "called by "
      in
      printf "- %s%s\n" called_by (Dyn.to_string (Stack_frame.to_dyn frame)))

let print_result arg res =
  let res =
    Result.map_error res
      ~f:
        (List.map
           ~f:
             (Exn_with_backtrace.map ~f:(fun exn ->
                  match exn with
                  | Memo.Cycle_error.E error ->
                    print_cycle_error error;
                    exn
                  | _ -> exn)))
  in
  let open Dyn.Encoder in
  Format.printf "f %d = %a@." arg Pp.to_fmt
    (Dyn.pp (Result.to_dyn int (list Exn_with_backtrace.to_dyn) res))

let run_collect_errors f =
  let open Fiber.O in
  Fiber.collect_errors (fun () -> Memo.Build.run (f ())) >>| function
  | Ok _ as res -> res
  | Error errs ->
    Error
      (List.map errs ~f:(fun (e : Exn_with_backtrace.t) ->
           match e.exn with
           | Memo.Error.E err -> { e with exn = Memo.Error.get err }
           | _ -> e))

let evaluate_and_print f x =
  let res =
    try
      Fiber.run
        ~iter:(fun () -> raise Exit)
        (run_collect_errors (fun () -> Memo.exec f x))
    with
    | exn -> Error [ Exn_with_backtrace.capture exn ]
  in
  print_result x res

let%expect_test "error handling and memo" =
  let f =
    int_fn_create "f" ~cutoff:Int.equal (fun x ->
        printf "Calling f %d\n" x;
        if x = 42 then
          failwith "42"
        else if x = 84 then
          Memo.Build.fork_and_join_unit
            (fun () -> failwith "left")
            (fun () -> failwith "right")
        else
          Memo.Build.return x)
  in
  let test x = evaluate_and_print f x in
  test 20;
  test 20;
  test 42;
  test 42;
  test 84;
  test 84;
  [%expect
    {|
    Calling f 20
    f 20 = Ok 20
    f 20 = Ok 20
    Calling f 42
    f 42 = Error [ { exn = "(Failure 42)"; backtrace = "" } ]
    f 42 = Error [ { exn = "(Failure 42)"; backtrace = "" } ]
    Calling f 84
    f 84 = Error
             [ { exn = "(Failure left)"; backtrace = "" }
             ; { exn = "(Failure right)"; backtrace = "" }
             ]
    f 84 = Error
             [ { exn = "(Failure left)"; backtrace = "" }
             ; { exn = "(Failure right)"; backtrace = "" }
             ] |}]

(* A test function counting runs. *)
let count_runs name =
  let counter = ref 0 in
  fun () ->
    printf "Started evaluating %s\n" name;
    incr counter;
    let result = !counter in
    let+ (_ : Run.t) = Memo.current_run () in
    printf "Evaluated %s: %d\n" name result;
    result

(* A test function incrementing a given memo. *)
let increment which which_memo () =
  printf "Started evaluating %s\n" which;
  let+ input = Memo.exec which_memo () in
  let result = input + 1 in
  printf "Evaluated %s: %d\n" which result;
  result

(* Create a memoization node with or without cutoff. *)
let create ~with_cutoff name f =
  let cutoff = Option.some_if with_cutoff Int.equal in
  Memo.create name ~input:(module Unit) ?cutoff f

let%expect_test "diamond with non-uniform cutoff structure" =
  let base = create ~with_cutoff:true "base" (count_runs "base") in
  let length_of_base which () =
    printf "Started evaluating %s\n" which;
    let+ base = Memo.exec base () in
    let result = String.length (Int.to_string base) in
    printf "Evaluated %s: %d\n" which result;
    result
  in
  let no_cutoff =
    create ~with_cutoff:false "no_cutoff" (length_of_base "no_cutoff")
  in
  let yes_cutoff =
    create ~with_cutoff:true "yes_cutoff" (length_of_base "yes_cutoff")
  in
  let after_no_cutoff =
    create ~with_cutoff:true "after_no_cutoff"
      (increment "after_no_cutoff" no_cutoff)
  in
  let after_yes_cutoff =
    create ~with_cutoff:true "after_yes_cutoff"
      (increment "after_yes_cutoff" yes_cutoff)
  in
  let summit offset =
    printf "Started evaluating summit with offset %d\n" offset;
    let+ after_no_cutoff, after_yes_cutoff =
      let* x = Memo.exec after_no_cutoff () in
      let+ y = Memo.exec after_yes_cutoff () in
      (x, y)
    in
    let result = after_no_cutoff + after_yes_cutoff + offset in
    printf "Evaluated summit with offset %d: %d\n" offset result;
    result
  in
  let summit = Memo.create "summit" ~input:(module Int) summit in
  Memo.Perf_counters.reset ();
  evaluate_and_print summit 0;
  print_perf_counters ();
  [%expect
    {|
    Started evaluating summit with offset 0
    Started evaluating after_no_cutoff
    Started evaluating no_cutoff
    Started evaluating base
    Evaluated base: 1
    Evaluated no_cutoff: 1
    Evaluated after_no_cutoff: 2
    Started evaluating after_yes_cutoff
    Started evaluating yes_cutoff
    Evaluated yes_cutoff: 1
    Evaluated after_yes_cutoff: 2
    Evaluated summit with offset 0: 4
    f 0 = Ok 4
    Memo: 7/7 computed/total nodes, 7/7 traversed/total edges
    Memo's cycle detection graph: 0/0 nodes/edges
  |}];
  Memo.Perf_counters.reset ();
  evaluate_and_print summit 1;
  print_perf_counters ();
  [%expect
    {|
    Started evaluating summit with offset 1
    Evaluated summit with offset 1: 5
    f 1 = Ok 5
    Memo: 1/1 computed/total nodes, 2/2 traversed/total edges
    Memo's cycle detection graph: 0/0 nodes/edges
  |}];
  Memo.reset Memo.Invalidation.empty;
  evaluate_and_print summit 0;
  print_perf_counters ();
  [%expect
    {|
    Started evaluating base
    Evaluated base: 2
    Started evaluating after_no_cutoff
    Started evaluating no_cutoff
    Evaluated no_cutoff: 1
    Evaluated after_no_cutoff: 2
    Started evaluating yes_cutoff
    Evaluated yes_cutoff: 1
    f 0 = Ok 4
    Memo: 5/7 computed/total nodes, 11/7 traversed/total edges
    Memo's cycle detection graph: 0/0 nodes/edges
  |}];
  Memo.Perf_counters.reset ();
  evaluate_and_print summit 1;
  print_perf_counters ();
  [%expect
    {|
    f 1 = Ok 5
    Memo: 0/1 computed/total nodes, 2/2 traversed/total edges
    Memo's cycle detection graph: 0/0 nodes/edges
  |}];
  Memo.Perf_counters.reset ();
  evaluate_and_print summit 2;
  print_perf_counters ();
  [%expect
    {|
    Started evaluating summit with offset 2
    Evaluated summit with offset 2: 6
    f 2 = Ok 6
    Memo: 1/1 computed/total nodes, 2/2 traversed/total edges
    Memo's cycle detection graph: 0/0 nodes/edges
  |}]

(* The test below sets up the following situation:

   - In the initial run, there are no dependency cycles.

   - In the second run, [base_or_summit] gets an additional dynamic dependency
   and eventually cycles back to itself.

   - In all subsequent runs, we are back to having no dependency cycles.

   The dependency chains in the new test have alternating cutoff/no-cutoff
   structure, to make sure that cycle detection can handle such cases. *)
let%expect_test "dynamic cycles with non-uniform cutoff structure" =
  let base = create ~with_cutoff:true "base" (count_runs "base") in
  let first_base_then_summit which ~summit_fdecl () =
    printf "Started evaluating %s\n" which;
    let* base = Memo.exec base () in
    match base with
    | input when input = 2 ->
      let summit = Fdecl.get summit_fdecl in
      printf "Cycling to summit from %s...\n" which;
      let+ result = Memo.exec summit input in
      printf "Miraculously evaluated %s: %d\n" which result;
      result
    | input ->
      printf "Evaluated %s: %d\n" which input;
      Build.return input
  in
  let rec incrementing_chain ~end_with_cutoff ~from n =
    match n with
    | 0 -> from
    | _ ->
      let from =
        incrementing_chain ~end_with_cutoff:(not end_with_cutoff) ~from (n - 1)
      in
      let cutoff =
        match end_with_cutoff with
        | false -> "_no_cutoff"
        | true -> "_yes_cutoff"
      in
      let name = "incrementing_chain_" ^ Int.to_string n ^ cutoff in
      create ~with_cutoff:end_with_cutoff name (increment name from)
  in
  let incrementing_chain_plus_input ~end_with_cutoff ~from =
    let chain =
      incrementing_chain ~end_with_cutoff:(not end_with_cutoff) ~from 4
    in
    let plus_input input =
      printf "Started evaluating the summit with input %d\n" input;
      let+ result = Memo.exec chain () in
      let result = result + input in
      printf "Evaluated the summit with input %d: %d\n" input result;
      result
    in
    let cutoff = Option.some_if end_with_cutoff Int.equal in
    Memo.create "incrementing_chain_plus_input"
      ~input:(module Int)
      ?cutoff plus_input
  in
  let summit_fdecl = Fdecl.create (fun _ -> Dyn.Opaque) in
  let cycle_creator_no_cutoff =
    create ~with_cutoff:false "cycle_creator_no_cutoff"
      (first_base_then_summit "cycle_creator_no_cutoff" ~summit_fdecl)
  in
  let summit_no_cutoff =
    incrementing_chain_plus_input ~end_with_cutoff:false
      ~from:cycle_creator_no_cutoff
  in
  Fdecl.set summit_fdecl summit_no_cutoff;
  let summit_fdecl = Fdecl.create (fun _ -> Dyn.Opaque) in
  let cycle_creator_yes_cutoff =
    create ~with_cutoff:true "cycle_creator_yes_cutoff"
      (first_base_then_summit "cycle_creator_yes_cutoff" ~summit_fdecl)
  in
  let summit_yes_cutoff =
    incrementing_chain_plus_input ~end_with_cutoff:true
      ~from:cycle_creator_yes_cutoff
  in
  Fdecl.set summit_fdecl summit_yes_cutoff;
  (* Calling [Memo.exec] and then not running the resulting [Fiber.t] used to
     bring the memoization framework into an inconsistent internal state, due to
     the eager execution of some internal side effects. That further manifested
     in deadlocks and reappearance of zombie computations. The problem has now
     been fixed and so the line below is just a no-op. *)
  Memo.Perf_counters.reset ();
  let _ = Memo.exec cycle_creator_no_cutoff () in
  print_perf_counters ();
  [%expect
    {|
    Memo: 0/0 computed/total nodes, 0/0 traversed/total edges
    Memo's cycle detection graph: 0/0 nodes/edges
  |}];
  Memo.Perf_counters.reset ();
  evaluate_and_print summit_no_cutoff 0;
  print_perf_counters ();
  [%expect
    {|
    Started evaluating the summit with input 0
    Started evaluating incrementing_chain_4_yes_cutoff
    Started evaluating incrementing_chain_3_no_cutoff
    Started evaluating incrementing_chain_2_yes_cutoff
    Started evaluating incrementing_chain_1_no_cutoff
    Started evaluating cycle_creator_no_cutoff
    Started evaluating base
    Evaluated base: 1
    Evaluated cycle_creator_no_cutoff: 1
    Evaluated incrementing_chain_1_no_cutoff: 2
    Evaluated incrementing_chain_2_yes_cutoff: 3
    Evaluated incrementing_chain_3_no_cutoff: 4
    Evaluated incrementing_chain_4_yes_cutoff: 5
    Evaluated the summit with input 0: 5
    f 0 = Ok 5
    Memo: 7/7 computed/total nodes, 7/7 traversed/total edges
    Memo's cycle detection graph: 0/0 nodes/edges
  |}];
  Memo.Perf_counters.reset ();
  evaluate_and_print summit_yes_cutoff 0;
  print_perf_counters ();
  [%expect
    {|
    Started evaluating the summit with input 0
    Started evaluating incrementing_chain_4_no_cutoff
    Started evaluating incrementing_chain_3_yes_cutoff
    Started evaluating incrementing_chain_2_no_cutoff
    Started evaluating incrementing_chain_1_yes_cutoff
    Started evaluating cycle_creator_yes_cutoff
    Evaluated cycle_creator_yes_cutoff: 1
    Evaluated incrementing_chain_1_yes_cutoff: 2
    Evaluated incrementing_chain_2_no_cutoff: 3
    Evaluated incrementing_chain_3_yes_cutoff: 4
    Evaluated incrementing_chain_4_no_cutoff: 5
    Evaluated the summit with input 0: 5
    f 0 = Ok 5
    Memo: 6/6 computed/total nodes, 6/6 traversed/total edges
    Memo's cycle detection graph: 0/0 nodes/edges
  |}];
  Memo.Perf_counters.reset ();
  evaluate_and_print summit_no_cutoff 2;
  print_perf_counters ();
  [%expect
    {|
    Started evaluating the summit with input 2
    Evaluated the summit with input 2: 7
    f 2 = Ok 7
    Memo: 1/1 computed/total nodes, 1/1 traversed/total edges
    Memo's cycle detection graph: 0/0 nodes/edges
  |}];
  Memo.Perf_counters.reset ();
  evaluate_and_print summit_yes_cutoff 2;
  print_perf_counters ();
  [%expect
    {|
    Started evaluating the summit with input 2
    Evaluated the summit with input 2: 7
    f 2 = Ok 7
    Memo: 1/1 computed/total nodes, 1/1 traversed/total edges
    Memo's cycle detection graph: 0/0 nodes/edges
  |}];
  Memo.reset Memo.Invalidation.empty;
  evaluate_and_print summit_no_cutoff 0;
  print_perf_counters ();
  [%expect
    {|
    Started evaluating base
    Evaluated base: 2
    Started evaluating incrementing_chain_2_yes_cutoff
    Started evaluating incrementing_chain_1_no_cutoff
    Started evaluating cycle_creator_no_cutoff
    Cycling to summit from cycle_creator_no_cutoff...
    Started evaluating incrementing_chain_4_yes_cutoff
    Started evaluating incrementing_chain_3_no_cutoff
    Started evaluating the summit with input 0
    Dependency cycle detected:
    - ("incrementing_chain_plus_input", 2)
    - called by ("cycle_creator_no_cutoff", ())
    - called by ("incrementing_chain_1_no_cutoff", ())
    - called by ("incrementing_chain_2_yes_cutoff", ())
    - called by ("incrementing_chain_3_no_cutoff", ())
    - called by ("incrementing_chain_4_yes_cutoff", ())
    f 0 = Error
            [ { exn =
                  "Cycle_error.E\n\
                  \  [ (\"incrementing_chain_plus_input\", 2)\n\
                  \  ; (\"cycle_creator_no_cutoff\", ())\n\
                  \  ; (\"incrementing_chain_1_no_cutoff\", ())\n\
                  \  ; (\"incrementing_chain_2_yes_cutoff\", ())\n\
                  \  ; (\"incrementing_chain_3_no_cutoff\", ())\n\
                  \  ; (\"incrementing_chain_4_yes_cutoff\", ())\n\
                  \  ]"
              ; backtrace = ""
              }
            ]
    Memo: 9/9 computed/total nodes, 16/8 traversed/total edges
    Memo's cycle detection graph: 5/5 nodes/edges
  |}];
  Memo.Perf_counters.reset ();
  evaluate_and_print summit_yes_cutoff 0;
  print_perf_counters ();
  [%expect
    {|
    Started evaluating cycle_creator_yes_cutoff
    Cycling to summit from cycle_creator_yes_cutoff...
    Started evaluating incrementing_chain_1_yes_cutoff
    Started evaluating incrementing_chain_3_yes_cutoff
    Started evaluating incrementing_chain_2_no_cutoff
    Started evaluating the summit with input 0
    Started evaluating incrementing_chain_4_no_cutoff
    Dependency cycle detected:
    - ("incrementing_chain_plus_input", 2)
    - called by ("cycle_creator_yes_cutoff", ())
    - called by ("incrementing_chain_1_yes_cutoff", ())
    - called by ("incrementing_chain_2_no_cutoff", ())
    - called by ("incrementing_chain_3_yes_cutoff", ())
    - called by ("incrementing_chain_4_no_cutoff", ())
    f 0 = Error
            [ { exn =
                  "Cycle_error.E\n\
                  \  [ (\"incrementing_chain_plus_input\", 2)\n\
                  \  ; (\"cycle_creator_yes_cutoff\", ())\n\
                  \  ; (\"incrementing_chain_1_yes_cutoff\", ())\n\
                  \  ; (\"incrementing_chain_2_no_cutoff\", ())\n\
                  \  ; (\"incrementing_chain_3_yes_cutoff\", ())\n\
                  \  ; (\"incrementing_chain_4_no_cutoff\", ())\n\
                  \  ]"
              ; backtrace = ""
              }
            ]
    Memo: 7/7 computed/total nodes, 14/7 traversed/total edges
    Memo's cycle detection graph: 5/5 nodes/edges
  |}];
  Memo.Perf_counters.reset ();
  evaluate_and_print summit_no_cutoff 2;
  print_perf_counters ();
  [%expect
    {|
    Dependency cycle detected:
    - ("incrementing_chain_4_yes_cutoff", ())
    - called by ("incrementing_chain_plus_input", 2)
    - called by ("cycle_creator_no_cutoff", ())
    - called by ("incrementing_chain_1_no_cutoff", ())
    - called by ("incrementing_chain_2_yes_cutoff", ())
    - called by ("incrementing_chain_3_no_cutoff", ())
    f 2 = Error
            [ { exn =
                  "Cycle_error.E\n\
                  \  [ (\"incrementing_chain_4_yes_cutoff\", ())\n\
                  \  ; (\"incrementing_chain_plus_input\", 2)\n\
                  \  ; (\"cycle_creator_no_cutoff\", ())\n\
                  \  ; (\"incrementing_chain_1_no_cutoff\", ())\n\
                  \  ; (\"incrementing_chain_2_yes_cutoff\", ())\n\
                  \  ; (\"incrementing_chain_3_no_cutoff\", ())\n\
                  \  ]"
              ; backtrace = ""
              }
            ]
    Memo: 0/0 computed/total nodes, 0/0 traversed/total edges
    Memo's cycle detection graph: 0/0 nodes/edges
  |}];
  Memo.Perf_counters.reset ();
  evaluate_and_print summit_yes_cutoff 2;
  print_perf_counters ();
  [%expect
    {|
    Dependency cycle detected:
    - ("incrementing_chain_4_no_cutoff", ())
    - called by ("incrementing_chain_plus_input", 2)
    - called by ("cycle_creator_yes_cutoff", ())
    - called by ("incrementing_chain_1_yes_cutoff", ())
    - called by ("incrementing_chain_2_no_cutoff", ())
    - called by ("incrementing_chain_3_yes_cutoff", ())
    f 2 = Error
            [ { exn =
                  "Cycle_error.E\n\
                  \  [ (\"incrementing_chain_4_no_cutoff\", ())\n\
                  \  ; (\"incrementing_chain_plus_input\", 2)\n\
                  \  ; (\"cycle_creator_yes_cutoff\", ())\n\
                  \  ; (\"incrementing_chain_1_yes_cutoff\", ())\n\
                  \  ; (\"incrementing_chain_2_no_cutoff\", ())\n\
                  \  ; (\"incrementing_chain_3_yes_cutoff\", ())\n\
                  \  ]"
              ; backtrace = ""
              }
            ]
    Memo: 0/0 computed/total nodes, 0/0 traversed/total edges
    Memo's cycle detection graph: 0/0 nodes/edges
  |}];
  Memo.reset Memo.Invalidation.empty;
  evaluate_and_print summit_no_cutoff 0;
  print_perf_counters ();
  [%expect
    {|
    Started evaluating base
    Evaluated base: 3
    Started evaluating incrementing_chain_2_yes_cutoff
    Started evaluating incrementing_chain_1_no_cutoff
    Started evaluating cycle_creator_no_cutoff
    Evaluated cycle_creator_no_cutoff: 3
    Evaluated incrementing_chain_1_no_cutoff: 4
    Evaluated incrementing_chain_2_yes_cutoff: 5
    Started evaluating incrementing_chain_4_yes_cutoff
    Started evaluating incrementing_chain_3_no_cutoff
    Evaluated incrementing_chain_3_no_cutoff: 6
    Evaluated incrementing_chain_4_yes_cutoff: 7
    Started evaluating the summit with input 0
    Evaluated the summit with input 0: 7
    f 0 = Ok 7
    Memo: 8/8 computed/total nodes, 14/7 traversed/total edges
    Memo's cycle detection graph: 0/0 nodes/edges
  |}];
  Memo.Perf_counters.reset ();
  evaluate_and_print summit_yes_cutoff 0;
  print_perf_counters ();
  [%expect
    {|
    Started evaluating cycle_creator_yes_cutoff
    Evaluated cycle_creator_yes_cutoff: 3
    Started evaluating incrementing_chain_1_yes_cutoff
    Evaluated incrementing_chain_1_yes_cutoff: 4
    Started evaluating incrementing_chain_3_yes_cutoff
    Started evaluating incrementing_chain_2_no_cutoff
    Evaluated incrementing_chain_2_no_cutoff: 5
    Evaluated incrementing_chain_3_yes_cutoff: 6
    Started evaluating the summit with input 0
    Started evaluating incrementing_chain_4_no_cutoff
    Evaluated incrementing_chain_4_no_cutoff: 7
    Evaluated the summit with input 0: 7
    f 0 = Ok 7
    Memo: 6/6 computed/total nodes, 12/6 traversed/total edges
    Memo's cycle detection graph: 0/0 nodes/edges
  |}];
  Memo.Perf_counters.reset ();
  evaluate_and_print summit_no_cutoff 2;
  print_perf_counters ();
  [%expect
    {|
    Started evaluating the summit with input 2
    Evaluated the summit with input 2: 9
    f 2 = Ok 9
    Memo: 1/1 computed/total nodes, 1/1 traversed/total edges
    Memo's cycle detection graph: 0/0 nodes/edges
  |}];
  Memo.Perf_counters.reset ();
  evaluate_and_print summit_yes_cutoff 2;
  print_perf_counters ();
  [%expect
    {|
    Started evaluating the summit with input 2
    Evaluated the summit with input 2: 9
    f 2 = Ok 9
    Memo: 1/1 computed/total nodes, 1/1 traversed/total edges
    Memo's cycle detection graph: 0/0 nodes/edges
  |}]

let%expect_test "deadlocks when creating a cycle twice" =
  let fdecl_base = Fdecl.create (fun _ -> Dyn.Opaque) in
  let cycle_creator =
    create ~with_cutoff:true "cycle_creator" (fun () ->
        printf "Started evaluating cycle_creator\n";
        let base = Fdecl.get fdecl_base in
        let+ result =
          let+ bases =
            Build.of_reproducible_fiber
              (Fiber.parallel_map [ (); () ] ~f:(fun () ->
                   Build.run (Memo.exec base ())))
          in
          match bases with
          | [ base1; base2 ] -> base1 + base2
          | _ -> assert false
        in
        printf "Miraculously evaluated cycle_creator: %d\n" result;
        result)
  in
  let base =
    create ~with_cutoff:true "base" (fun () ->
        printf "Started evaluating base\n";
        let+ result = Memo.exec cycle_creator () in
        printf "Miraculously evaluated base: %d\n" result;
        result)
  in
  Fdecl.set fdecl_base base;
  let middle =
    create ~with_cutoff:true "middle" (fun () ->
        printf "Started evaluating middle\n";
        let+ result = Memo.exec base () in
        printf "Miraculously evaluated middle: %d\n" result;
        result)
  in
  let summit =
    Memo.create "summit"
      ~input:(module Int)
      (fun offset ->
        printf "Started evaluating summit\n";
        let+ middle = Memo.exec middle () in
        let result = middle + offset in
        printf "Miraculously evaluated summit: %d\n" result;
        result)
  in
  evaluate_and_print summit 0;
  evaluate_and_print summit 1;
  [%expect
    {|
    Started evaluating summit
    Started evaluating middle
    Started evaluating base
    Started evaluating cycle_creator
    f 0 = Error [ { exn = "Exit"; backtrace = "" } ]
    Started evaluating summit
    f 1 = Error [ { exn = "Exit"; backtrace = "" } ]
  |}];
  Memo.reset Memo.Invalidation.empty;
  evaluate_and_print summit 0;
  evaluate_and_print summit 2;
  [%expect
    {|
    Started evaluating summit
    Started evaluating middle
    Started evaluating base
    Started evaluating cycle_creator
    f 0 = Error [ { exn = "Exit"; backtrace = "" } ]
    Started evaluating summit
    f 2 = Error [ { exn = "Exit"; backtrace = "" } ]
  |}]

let lazy_rec ~name f =
  let fdecl = Fdecl.create (fun _ -> Dyn.Opaque) in
  let node = Memo.Lazy.create ~name (fun () -> f (Fdecl.get fdecl)) in
  Fdecl.set fdecl node;
  node

let%expect_test "two similar, but not physically-equal, cycle errors" =
  let cycle1 = lazy_rec ~name:"cycle" (fun node -> Memo.Lazy.force node) in
  let cycle2 = lazy_rec ~name:"cycle" (fun node -> Memo.Lazy.force node) in
  let both =
    Memo.Lazy.create ~name:"both" (fun () ->
        Memo.Build.fork_and_join_unit
          (fun () -> Lazy.force cycle1)
          (fun () -> Lazy.force cycle2))
  in
  run_and_log_errors (Memo.Lazy.force both);
  (* Even though these errors look similar, they are actually talking about two
     different cycles which can be distinguished by the internal node ids, so
     they are not deduplicated. *)
  [%expect
    {|
    Error: { exn =
               "Memo.Error.E\n\
               \  { exn = \"Cycle_error.E [ (\\\"cycle\\\", ()) ]\"; stack = [ (\"both\", ()) ] }"
           ; backtrace = ""
           }
    Error: { exn =
               "Memo.Error.E\n\
               \  { exn = \"Cycle_error.E [ (\\\"cycle\\\", ()) ]\"; stack = [ (\"both\", ()) ] }"
           ; backtrace = ""
           }
  |}]

let%expect_test "Nested nodes with cutoff are recomputed optimally" =
  let counter = create ~with_cutoff:false "counter" (count_runs "counter") in
  let summit =
    Memo.create "summit"
      ~input:(module Int)
      (fun offset ->
        printf "Started evaluating summit\n";
        let middle =
          create ~with_cutoff:false "middle" (fun () ->
              printf "Started evaluating middle\n";
              let base =
                create ~with_cutoff:false "base" (fun () ->
                    printf "Started evaluating base\n";
                    let+ result = Memo.exec counter () in
                    printf "Evaluated middle: %d\n" result;
                    result)
              in
              let+ result = Memo.exec base () in
              printf "Evaluated middle: %d\n" result;
              result)
        in
        let+ middle = Memo.exec middle () in
        let result = middle + offset in
        printf "Evaluated summit: %d\n" result;
        result)
  in
  Memo.Perf_counters.reset ();
  evaluate_and_print summit 0;
  evaluate_and_print summit 1;
  print_perf_counters ();
  (* In the first run, everything is OK. *)
  [%expect
    {|
    Started evaluating summit
    Started evaluating middle
    Started evaluating base
    Started evaluating counter
    Evaluated counter: 1
    Evaluated middle: 1
    Evaluated middle: 1
    Evaluated summit: 1
    f 0 = Ok 1
    Started evaluating summit
    Started evaluating middle
    Started evaluating base
    Evaluated middle: 1
    Evaluated middle: 1
    Evaluated summit: 2
    f 1 = Ok 2
    Memo: 8/8 computed/total nodes, 7/7 traversed/total edges
    Memo's cycle detection graph: 0/0 nodes/edges
  |}];
  Memo.reset Memo.Invalidation.empty;
  evaluate_and_print summit 0;
  evaluate_and_print summit 2;
  print_perf_counters ();
  (* In the second run, we don't recompute [base] three times as we did before. *)
  [%expect
    {|
    Started evaluating summit
    Started evaluating middle
    Started evaluating base
    Started evaluating counter
    Evaluated counter: 2
    Evaluated middle: 2
    Evaluated middle: 2
    Evaluated summit: 2
    f 0 = Ok 2
    Started evaluating summit
    Started evaluating middle
    Started evaluating base
    Evaluated middle: 2
    Evaluated middle: 2
    Evaluated summit: 4
    f 2 = Ok 4
    Memo: 8/10 computed/total nodes, 11/7 traversed/total edges
    Memo's cycle detection graph: 0/0 nodes/edges
  |}]

(* In addition to its direct purpose, this test also: (i) demonstrates what
   happens in the presence of non-determinism; and (ii) tests cell invalidation. *)
let%expect_test "Test that there are no phantom dependencies" =
  let counter = ref 0 in
  let const_8 =
    create ~with_cutoff:false "base" (fun () ->
        let result = 8 in
        printf "base = %d\n" result;
        Build.return result)
  in
  let cell = Memo.cell const_8 () in
  let summit =
    Memo.create "summit"
      ~input:(module Int)
      (fun offset ->
        printf "Started evaluating summit\n";
        let middle =
          create ~with_cutoff:false "middle" (fun () ->
              incr counter;
              match !counter with
              | 1 ->
                printf "*** middle depends on base ***\n";
                Memo.Cell.read cell
              | _ ->
                printf "*** middle does not depend on base ***\n";
                Build.return 0)
        in
        let+ middle = Memo.exec middle () in
        let result = middle + offset in
        printf "Evaluated summit: %d\n" result;
        result)
  in
  evaluate_and_print summit 0;
  [%expect
    {|
    Started evaluating summit
    *** middle depends on base ***
    base = 8
    Evaluated summit: 8
    f 0 = Ok 8
  |}];
  Memo.reset Memo.Invalidation.empty;
  evaluate_and_print summit 0;
  (* No recomputation is needed since the [cell] is up to date. *)
  [%expect {| f 0 = Ok 8 |}];
  Memo.reset (Memo.Cell.invalidate cell);
  evaluate_and_print summit 0;
  (* Note that we no longer depend on the [cell]. *)
  [%expect
    {|
    Started evaluating summit
    *** middle does not depend on base ***
    Evaluated summit: 0
    f 0 = Ok 0 |}];
  Memo.reset (Memo.Cell.invalidate cell);
  evaluate_and_print summit 0;
  (* Nothing is recomputed, since the result no longer depends on the cell. In
     the past, the cell remained as a "phantom dependency", which caused
     unnecessary recomputations. *)
  [%expect {| f 0 = Ok 0 |}]

let%expect_test "Abandoned node with no cutoff is recomputed" =
  let count_runs = count_runs "base" in
  let which_base = ref 0 in
  let base () =
    incr which_base;
    printf "Created base #%d\n" !which_base;
    create ~with_cutoff:false "base" count_runs
  in
  let last_created_base = ref None in
  let captured_base = ref None in
  let middle =
    Memo.create "middle"
      ~input:(module Unit)
      (fun () ->
        printf "Started evaluating middle\n";
        let base = base () in
        last_created_base := Some base;
        let+ result = Memo.exec base () in
        printf "Evaluated middle: %d\n" result;
        result)
  in
  let summit =
    Memo.create "summit"
      ~input:(module Int)
      (fun input ->
        printf "Started evaluating summit\n";
        let* middle = Memo.exec middle () in
        let+ result =
          match middle with
          | 1 ->
            printf "*** Captured last base ***\n";
            captured_base := !last_created_base;
            Memo.exec (Option.value_exn !captured_base) ()
          | 2 ->
            printf "*** Abandoned captured base ***\n";
            Build.return input
          | _ ->
            printf "*** Recalled captured base ***\n";
            Memo.exec (Option.value_exn !captured_base) ()
        in
        printf "Evaluated summit: %d\n" result;
        result)
  in
  Memo.reset Memo.Invalidation.empty;
  evaluate_and_print summit 0;
  print_perf_counters ();
  [%expect
    {|
    Started evaluating summit
    Started evaluating middle
    Created base #1
    Started evaluating base
    Evaluated base: 1
    Evaluated middle: 1
    *** Captured last base ***
    Evaluated summit: 1
    f 0 = Ok 1
    Memo: 4/4 computed/total nodes, 4/4 traversed/total edges
    Memo's cycle detection graph: 0/0 nodes/edges
  |}];
  Memo.reset Memo.Invalidation.empty;
  evaluate_and_print summit 0;
  print_perf_counters ();
  [%expect
    {|
    Started evaluating summit
    Started evaluating middle
    Created base #2
    Started evaluating base
    Evaluated base: 2
    Evaluated middle: 2
    *** Abandoned captured base ***
    Evaluated summit: 0
    f 0 = Ok 0
    Memo: 4/5 computed/total nodes, 6/3 traversed/total edges
    Memo's cycle detection graph: 0/0 nodes/edges
  |}];
  (* At this point, [captured_base] is a stale computation: [restore_from_cache]
     failed but [compute] never started. *)
  Memo.reset Memo.Invalidation.empty;
  evaluate_and_print summit 0;
  print_perf_counters ();
  (* We will now attempt to force [compute] of a stale computation but this is
     handled correctly by restarting the computation. Note that this causes an
     additional increment of the counter, thus leading to an inconsistent value
     of [base] observed by the [middle] (3) and [summit] (4) nodes. *)
  [%expect
    {|
    Started evaluating summit
    Started evaluating middle
    Created base #3
    Started evaluating base
    Evaluated base: 3
    Evaluated middle: 3
    *** Recalled captured base ***
    Started evaluating base
    Evaluated base: 4
    Evaluated summit: 4
    f 0 = Ok 4
    Memo: 5/6 computed/total nodes, 9/5 traversed/total edges
    Memo's cycle detection graph: 0/0 nodes/edges
  |}]

let print_exns f =
  let res =
    match Fiber.run ~iter:(fun () -> raise Exit) (run_collect_errors f) with
    | Ok _ -> assert false
    | Error exns ->
      Error (List.map exns ~f:(fun (e : Exn_with_backtrace.t) -> e.exn))
    | exception exn -> Error [ exn ]
  in
  let open Dyn.Encoder in
  Format.printf "%a@." Pp.to_fmt
    (Dyn.pp (Result.to_dyn unit (list Exn.to_dyn) res))

let%expect_test "error handling with diamonds" =
  Printexc.record_backtrace true;
  let f_impl = Fdecl.create Dyn.Encoder.opaque in
  let f =
    int_fn_create "error-diamond: f" ~cutoff:Unit.equal (fun x ->
        Fdecl.get f_impl x)
  in
  Fdecl.set f_impl (fun x ->
      printf "Calling f %d\n" x;
      if x = 0 then
        failwith "reached 0"
      else
        Memo.Build.fork_and_join_unit
          (fun () -> Memo.exec f (x - 1))
          (fun () -> Memo.exec f (x - 1)));
  let test x = print_exns (fun () -> Memo.exec f x) in
  test 0;
  [%expect {|
    Calling f 0
    Error [ "(Failure \"reached 0\")" ]
  |}];
  test 1;
  [%expect {|
    Calling f 1
    Error [ "(Failure \"reached 0\")" ]
  |}];
  test 2;
  [%expect {|
    Calling f 2
    Error [ "(Failure \"reached 0\")" ]
  |}]

let%expect_test "error handling and duplicate exceptions" =
  Printexc.record_backtrace true;
  let f_impl = Fdecl.create Dyn.Encoder.opaque in
  let f =
    int_fn_create "test8: duplicate-exception: f" ~cutoff:Unit.equal (fun x ->
        Fdecl.get f_impl x)
  in
  let fail =
    int_fn_create "test8: fail" ~cutoff:Unit.equal (fun _x -> failwith "42")
  in
  let forward_fail =
    int_fn_create "test8: forward fail" ~cutoff:Unit.equal (fun x ->
        Memo.exec fail x)
  in
  let forward_fail2 =
    int_fn_create "test8: forward fail2" ~cutoff:Unit.equal (fun x ->
        Memo.exec fail x)
  in
  Fdecl.set f_impl (fun x ->
      printf "Calling f %d\n" x;
      match x with
      | 0 -> Memo.exec forward_fail x
      | 1 -> Memo.exec forward_fail2 x
      | _ ->
        Memo.Build.fork_and_join_unit
          (fun () -> Memo.exec f (x - 1))
          (fun () -> Memo.exec f (x - 2)));
  let test x = print_exns (fun () -> Memo.exec f x) in
  test 2;
  [%expect
    {|
    Calling f 2
    Calling f 1
    Calling f 0
    Error [ "(Failure 42)" ]
  |}]

let%expect_test "reproducible errors are cached" =
  Printexc.record_backtrace false;
  let f =
    Memo.create "area of a square"
      ~input:(module Int)
      (fun x ->
        printf "Started evaluating %d\n" x;
        if x < 0 then failwith (sprintf "Negative input %d" x);
        if x = 0 then raise (Memo.Non_reproducible (Failure "Zero input"));
        let res = x * x in
        printf "Evaluated %d: %d\n" x res;
        Memo.Build.return res)
  in
  Memo.Perf_counters.reset ();
  evaluate_and_print f 5;
  evaluate_and_print f (-5);
  evaluate_and_print f 0;
  (* Note that the [Non_reproducible] wrapper has been removed. *)
  print_perf_counters ();
  [%expect
    {|
    Started evaluating 5
    Evaluated 5: 25
    f 5 = Ok 25
    Started evaluating -5
    f -5 = Error [ { exn = "(Failure \"Negative input -5\")"; backtrace = "" } ]
    Started evaluating 0
    f 0 = Error [ { exn = "(Failure \"Zero input\")"; backtrace = "" } ]
    Memo: 3/3 computed/total nodes, 0/0 traversed/total edges
    Memo's cycle detection graph: 0/0 nodes/edges
  |}];
  Memo.Perf_counters.reset ();
  evaluate_and_print f 5;
  evaluate_and_print f (-5);
  evaluate_and_print f 0;
  print_perf_counters ();
  (* Note that we do not see any "Started evaluating" messages because both [Ok]
     and [Error] results have been cached. *)
  [%expect
    {|
    f 5 = Ok 25
    f -5 = Error [ { exn = "(Failure \"Negative input -5\")"; backtrace = "" } ]
    f 0 = Error [ { exn = "(Failure \"Zero input\")"; backtrace = "" } ]
    Memo: 0/0 computed/total nodes, 0/0 traversed/total edges
    Memo's cycle detection graph: 0/0 nodes/edges
  |}];
  Memo.reset Memo.Invalidation.empty;
  evaluate_and_print f 5;
  evaluate_and_print f (-5);
  evaluate_and_print f 0;
  print_perf_counters ();
  (* Here we re-execute only one computation: the one that corresponds to the
     non-reproducible error. *)
  [%expect
    {|
    f 5 = Ok 25
    f -5 = Error [ { exn = "(Failure \"Negative input -5\")"; backtrace = "" } ]
    Started evaluating 0
    f 0 = Error [ { exn = "(Failure \"Zero input\")"; backtrace = "" } ]
    Memo: 1/3 computed/total nodes, 0/0 traversed/total edges
    Memo's cycle detection graph: 0/0 nodes/edges
  |}]

let%expect_test "errors work with early cutoff" =
  let divide =
    let exception Input_too_large of Memo.Run.t in
    let first_run = Memo.Run.For_tests.current () in
    Printexc.register_printer (fun exn ->
        match exn with
        | Input_too_large run ->
          Some
            (sprintf "Input_too_large <%s run>"
               (if Memo.Run.For_tests.compare first_run run = Eq then
                 "first"
               else
                 "second"))
        | _ -> None);
    Memo.create "divide 100 by input"
      ~input:(module Int)
      ~cutoff:Int.equal
      (fun x ->
        let+ run = Memo.current_run () in
        printf "[divide] Started evaluating %d\n" x;
        if x > 100 then
          (* This exception will be different in each run. *)
          raise (Input_too_large run);
        let res = 100 / x in
        printf "[divide] Evaluated %d: %d\n" x res;
        res)
  in
  let f =
    Memo.create "Negate"
      ~input:(module Int)
      (fun x ->
        printf "[negate] Started evaluating %d\n" x;
        let+ res = Memo.exec divide x >>| Stdlib.Int.neg in
        printf "[negate] Evaluated %d: %d\n" x res;
        res)
  in
  Memo.Perf_counters.reset ();
  evaluate_and_print f 0;
  evaluate_and_print f 20;
  evaluate_and_print f 200;
  print_perf_counters ();
  [%expect
    {|
    [negate] Started evaluating 0
    [divide] Started evaluating 0
    f 0 = Error [ { exn = "Division_by_zero"; backtrace = "" } ]
    [negate] Started evaluating 20
    [divide] Started evaluating 20
    [divide] Evaluated 20: 5
    [negate] Evaluated 20: -5
    f 20 = Ok -5
    [negate] Started evaluating 200
    [divide] Started evaluating 200
    f 200 = Error [ { exn = "Input_too_large <first run>"; backtrace = "" } ]
    Memo: 7/7 computed/total nodes, 6/6 traversed/total edges
    Memo's cycle detection graph: 0/0 nodes/edges
  |}];
  Memo.reset Memo.Invalidation.empty;
  evaluate_and_print f 0;
  evaluate_and_print f 20;
  evaluate_and_print f 200;
  print_perf_counters ();
  (* Here we reevaluate all calls to [divide] because they depend on the current
     run. Due to the early cutoff, we skip recomputing the outer [negate] for
     the inputs 0 (error) and 20 (success), because the results remain the same.
     However, we do attempt to re-evaluate [negate] for the input 200 because
     the result of [divide] does change: we get a fresh exception. *)
  [%expect
    {|
    [divide] Started evaluating 0
    f 0 = Error [ { exn = "Division_by_zero"; backtrace = "" } ]
    [divide] Started evaluating 20
    [divide] Evaluated 20: 5
    f 20 = Ok -5
    [divide] Started evaluating 200
    [negate] Started evaluating 200
    f 200 = Error [ { exn = "Input_too_large <second run>"; backtrace = "" } ]
    Memo: 5/7 computed/total nodes, 10/6 traversed/total edges
    Memo's cycle detection graph: 0/0 nodes/edges
  |}]

(* This test uses non-deterministic tasks to show that adding old dependency
   edges to the cycle detection graph can lead to spurious cycle errors, where a
   cycle is formed by a combination of old and new edges.

   In the first build run, A depends on B. In all later runs, B depends on A. *)
let%expect_test "Test that there are no spurious cycles" =
  let task_b_fdecl = Fdecl.create (fun _ -> Dyn.Opaque) in
  let task_a =
    let memory_a = ref 0 in
    Memo.create "A"
      ~input:(module Int)
      (fun _input ->
        printf "Started evaluating A\n";
        let+ result =
          match !memory_a with
          | 0 ->
            let+ b = Memo.exec (Fdecl.get task_b_fdecl) 0 in
            b + 1
          | _ -> Memo.Build.return 0
        in
        incr memory_a;
        printf "A = %d\n" result;
        printf "Evaluated A\n";
        result)
  in
  let task_b =
    let memory_b = ref 0 in
    Memo.create "B"
      ~input:(module Int)
      ~cutoff:Int.equal
      (fun _input ->
        printf "Started evaluating B\n";
        let+ result =
          match !memory_b with
          | 0 -> Memo.Build.return 0
          | _ ->
            let+ a = Memo.exec task_a 0 in
            a + 1
        in
        incr memory_b;
        printf "B = %d\n" result;
        printf "Evaluated B\n";
        result)
  in
  Fdecl.set task_b_fdecl task_b;
  Memo.Perf_counters.reset ();
  evaluate_and_print task_a 0;
  [%expect
    {|
    Started evaluating A
    Started evaluating B
    B = 0
    Evaluated B
    A = 1
    Evaluated A
    f 0 = Ok 1
  |}];
  evaluate_and_print task_b 0;
  [%expect {| f 0 = Ok 0 |}];
  print_perf_counters ();
  [%expect
    {|
    Memo: 2/2 computed/total nodes, 1/1 traversed/total edges
    Memo's cycle detection graph: 0/0 nodes/edges
  |}];
  Memo.reset (Memo.Cell.invalidate (Memo.cell task_b 0));
  evaluate_and_print task_a 0;
  (* Note that here task B blows up with a cycle error when trying to restore
     its result from the cache. A doesn't need it and terminates correctly. *)
  [%expect
    {|
    Started evaluating B
    Started evaluating A
    A = 0
    Evaluated A
    f 0 = Ok 0
  |}];
  evaluate_and_print task_b 0;
  (* Now we get to see the spurious cycle. *)
  [%expect
    {|
    Dependency cycle detected:
    - ("A", 0)
    - called by ("B", 0)
    f 0 = Error
            [ { exn = "Cycle_error.E [ (\"A\", 0); (\"B\", 0) ]"
              ; backtrace = ""
              }
            ]
  |}]

let%expect_test "Test Memo.clear_cache" =
  let add_one =
    Memo.create "Add 1"
      ~input:(module Int)
      (fun input ->
        let result = input + 1 in
        printf "Evaluated add_one(%d)\n" input;
        Memo.Build.return result)
  in
  let add_two =
    Memo.create "Add 2"
      ~input:(module Int)
      (fun input ->
        let+ result = Memo.exec add_one input in
        printf "Evaluated add_two(%d)\n" input;
        result + 1)
  in
  Memo.Perf_counters.reset ();
  evaluate_and_print add_one 1;
  evaluate_and_print add_one 2;
  [%expect
    {|
    Evaluated add_one(1)
    f 1 = Ok 2
    Evaluated add_one(2)
    f 2 = Ok 3
  |}];
  evaluate_and_print add_two 1;
  evaluate_and_print add_two 2;
  [%expect
    {|
    Evaluated add_two(1)
    f 1 = Ok 3
    Evaluated add_two(2)
    f 2 = Ok 4
  |}];
  print_perf_counters ();
  [%expect
    {|
    Memo: 4/4 computed/total nodes, 2/2 traversed/total edges
    Memo's cycle detection graph: 0/0 nodes/edges
  |}];
  let invalidation = Memo.Invalidation.invalidate_cache add_one in
  Memo.reset invalidation;
  evaluate_and_print add_one 1;
  evaluate_and_print add_one 2;
  (* We recompute all [add_one] calls. *)
  [%expect
    {|
    Evaluated add_one(1)
    f 1 = Ok 2
    Evaluated add_one(2)
    f 2 = Ok 3
  |}];
  evaluate_and_print add_two 1;
  evaluate_and_print add_two 2;
  (* We recompute [add_two] calls because they depend on [add_one] calls. *)
  [%expect
    {|
    Evaluated add_two(1)
    f 1 = Ok 3
    Evaluated add_two(2)
    f 2 = Ok 4
  |}];
  print_perf_counters ();
  [%expect
    {|
    Memo: 4/4 computed/total nodes, 4/2 traversed/total edges
    Memo's cycle detection graph: 0/0 nodes/edges
  |}]

(* In the first run, the dependency structure is: A -> B -> C -> current run.

   In the second run, it's: A -> X -> B -> X, i.e. there is a dependency cycle.

   We force the computation of A and C in parallel, and make C yield, so that B
   gets blocked waiting for C to eventually complete. As a result, during the
   restore_from_cache phase in the second run, the cycle detection algorithm
   will add the path A -> B -> C to the DAG. Then, in the compute phase, B will
   get blocked on X, adding the path A -> X -> B -> X to the DAG, thus detecting
   the cycle. If the two phases are not cleanly separated, the second path might
   get cut down to just B -> X, missing the cycle and leading to a deadlock. *)
let%expect_test "restore_from_cache and compute phases are well-separated" =
  let task_c =
    Memo.create "C"
      ~input:(module Int)
      (fun input ->
        printf "Started evaluating C\n";
        let* () =
          Memo.Build.of_reproducible_fiber (Fiber.of_thunk Scheduler.yield)
        in
        let+ (_ : Run.t) = Memo.current_run () in
        printf "Evaluated C\n";
        input + 1)
  in
  let task_x_fdecl = Fdecl.create (fun _ -> Dyn.Opaque) in
  let task_b =
    let memory_b = ref 0 in
    Memo.create "B"
      ~input:(module Int)
      (fun input ->
        printf "Started evaluating B\n";
        let+ result =
          match !memory_b with
          | 0 -> Memo.exec task_c input
          | _ -> Memo.exec (Fdecl.get task_x_fdecl) input
        in
        incr memory_b;
        printf "B = %d\n" result;
        printf "Evaluated B\n";
        result)
  in
  let task_a =
    let memory_a = ref 0 in
    Memo.create "A"
      ~input:(module Int)
      (fun input ->
        printf "Started evaluating A\n";
        let+ result =
          match !memory_a with
          | 0 -> Memo.exec task_b input
          | _ -> Memo.exec (Fdecl.get task_x_fdecl) input
        in
        incr memory_a;
        printf "A = %d\n" result;
        printf "Evaluated A\n";
        result)
  in
  let task_x =
    Memo.create "X"
      ~input:(module Int)
      (fun input ->
        printf "Started evaluating X\n";
        let+ result = Memo.exec task_b input in
        printf "Evaluated X\n";
        result)
  in
  Fdecl.set task_x_fdecl task_x;
  Memo.Perf_counters.reset ();
  let (_results : int * int) =
    Scheduler.run
      (Fiber.fork_and_join
         (fun () -> Memo.Build.run (Memo.exec task_c 0))
         (fun () -> Memo.Build.run (Memo.exec task_a 0)))
  in
  [%expect
    {|
    Started evaluating C
    Started evaluating A
    Started evaluating B
    Evaluated C
    B = 1
    Evaluated B
    A = 1
    Evaluated A
  |}];
  print_perf_counters ();
  [%expect
    {|
    Memo: 4/4 computed/total nodes, 3/3 traversed/total edges
    Memo's cycle detection graph: 2/2 nodes/edges |}];
  Memo.reset Invalidation.empty;
  (match
     Scheduler.run
       (Fiber.fork_and_join
          (fun () -> Memo.Build.run (Memo.exec task_c 0))
          (fun () -> Memo.Build.run (Memo.exec task_a 0)))
   with
  | (_result : int * int) -> ()
  | exception Test_scheduler.Never -> print_endline "Deadlock!"
  | exception (Memo.Error.E error as exn) -> (
    match Memo.Error.get error with
    | Memo.Cycle_error.E error -> print_cycle_error error
    | _ -> raise exn));
  [%expect
    {|
    Started evaluating C
    Started evaluating A
    Started evaluating X
    Started evaluating B
    Dependency cycle detected:
    - ("B", 0)
    - called by ("X", 0)
  |}]
