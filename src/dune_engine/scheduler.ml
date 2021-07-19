open! Stdune
open Import
open Fiber.O

module Config = struct
  include Config

  module Display = struct
    type verbosity =
      | Quiet
      | Short
      | Verbose

    type t =
      { status_line : bool
      ; verbosity : verbosity
      }

    (* Even though [status_line] is true by default in most of these, the status
       line is actually not shown if the output is redirected to a file or a
       pipe. *)
    let all =
      [ ("progress", { verbosity = Quiet; status_line = true })
      ; ("verbose", { verbosity = Verbose; status_line = true })
      ; ("short", { verbosity = Short; status_line = true })
      ; ("quiet", { verbosity = Quiet; status_line = false })
      ]

    let verbosity_to_dyn : verbosity -> Dyn.t = function
      | Quiet -> Variant ("Quiet", [])
      | Short -> Variant ("Short", [])
      | Verbose -> Variant ("Verbose", [])

    let to_dyn { status_line; verbosity } : Dyn.t =
      Record
        [ ("status_line", Dyn.Bool status_line)
        ; ("verbosity", verbosity_to_dyn verbosity)
        ]

    let console_backend t =
      match t.status_line with
      | false -> Console.Backend.dumb
      | true -> Console.Backend.progress
  end

  type t =
    { concurrency : int
    ; display : Display.t
    ; rpc : Dune_rpc.Where.t option
    ; stats : Dune_stats.t option
    }

  let add_to_env t env =
    match t.rpc with
    | None -> env
    | Some where -> Dune_rpc.Where.add_to_env where env
end

type job =
  { pid : Pid.t
  ; ivar : Proc.Process_info.t Fiber.Ivar.t
  }

module Signal = struct
  type t =
    | Int
    | Quit
    | Term

  let compare : t -> t -> Ordering.t = Poly.compare

  include Comparable.Make (struct
    type nonrec t = t

    let compare = compare

    let to_dyn _ = Dyn.opaque
  end)

  let all = [ Int; Quit; Term ]

  let to_int = function
    | Int -> Sys.sigint
    | Quit -> Sys.sigquit
    | Term -> Sys.sigterm

  let of_int =
    List.map all ~f:(fun t -> (to_int t, t))
    |> Int.Map.of_list_reduce ~f:(fun _ t -> t)
    |> Int.Map.find

  let name t = Signal.name (to_int t)
end

module Thread = struct
  include Thread

  let block_signals =
    lazy
      (let signos = List.map Signal.all ~f:Signal.to_int in
       ignore (Unix.sigprocmask SIG_BLOCK signos : int list))

  let create =
    if Sys.win32 then
      Thread.create
    else
      (* On unix, we make sure to block signals globally before starting a
         thread so that only the signal watcher thread can receive signals. *)
      fun f x ->
    Lazy.force block_signals;
    Thread.create f x

  let spawn f =
    let (_ : Thread.t) = create f () in
    ()
end

(** The event queue *)
module Event : sig
  type build_input_change =
    | Sync
    | Fs_event of Dune_file_watcher.Fs_memo_event.t
    | Invalidation of Memo.Invalidation.t

  type t =
    | File_watcher_task of (unit -> Dune_file_watcher.Event.t list)
    | Build_inputs_changed of build_input_change Nonempty_list.t
    | File_system_watcher_terminated
    | Job_completed of job * Proc.Process_info.t
    | Signal of Signal.t
    | Worker_task of Fiber.fill
    | Yield of unit Fiber.Ivar.t
    | Timer of Fiber.fill

  module Queue : sig
    type t

    type event

    val create : Dune_stats.t option -> t

    (** Return the next event. File changes event are always flattened and
        returned first. *)
    val next : t -> event

    (** Pending worker tasks *)
    val pending_worker_tasks : t -> int

    (** Register the fact that a job was started. *)
    val register_job_started : t -> unit

    (** Number of jobs for which the status hasn't been reported yet .*)
    val pending_jobs : t -> int

    val send_worker_task_completed : t -> Fiber.fill -> unit

    val register_worker_task_started : t -> unit

    val send_file_watcher_task :
      t -> (unit -> Dune_file_watcher.Event.t list) -> unit

    (** It's a bit weird to have both this and [send_file_watcher_task]. The
        reason is that [send_file_watcher_task] uses [send_file_watcher_events]
        internally. *)
    val send_file_watcher_events : t -> Dune_file_watcher.Event.t list -> unit

    val send_invalidation_event : t -> Memo.Invalidation.t -> unit

    val send_job_completed : t -> job -> Proc.Process_info.t -> unit

    val send_signal : t -> Signal.t -> unit

    val send_timers_completed : t -> Fiber.fill Nonempty_list.t -> unit

    val yield_if_there_are_pending_events : t -> unit Fiber.t
  end
  with type event := t
end = struct
  type build_input_change =
    | Sync
    | Fs_event of Dune_file_watcher.Fs_memo_event.t
    | Invalidation of Memo.Invalidation.t

  type t =
    | File_watcher_task of (unit -> Dune_file_watcher.Event.t list)
    | Build_inputs_changed of build_input_change Nonempty_list.t
    | File_system_watcher_terminated
    | Job_completed of job * Proc.Process_info.t
    | Signal of Signal.t
    | Worker_task of Fiber.fill
    | Yield of unit Fiber.Ivar.t
    | Timer of Fiber.fill

  module Invalidation_event = struct
    type t =
      | Invalidation of Memo.Invalidation.t
      | Filesystem_event of Dune_file_watcher.Event.t
  end

  module Queue = struct
    type event = t

    type t =
      { jobs_completed : (job * Proc.Process_info.t) Queue.t
      ; file_watcher_tasks : (unit -> Dune_file_watcher.Event.t list) Queue.t
      ; mutable invalidation_events : Invalidation_event.t list
      ; mutable signals : Signal.Set.t
      ; mutex : Mutex.t
      ; cond : Condition.t
      ; mutable pending_jobs : int
      ; mutable pending_worker_tasks : int
      ; worker_tasks_completed : Fiber.fill Queue.t
      ; timers : Fiber.fill Queue.t
      ; stats : Dune_stats.t option
      ; mutable got_event : bool
      ; mutable yield : unit Fiber.Ivar.t option
      }

    let create stats =
      let jobs_completed = Queue.create () in
      let file_watcher_tasks = Queue.create () in
      let worker_tasks_completed = Queue.create () in
      let invalidation_events = [] in
      let signals = Signal.Set.empty in
      let mutex = Mutex.create () in
      let cond = Condition.create () in
      let pending_jobs = 0 in
      let pending_worker_tasks = 0 in
      let timers = Queue.create () in
      { jobs_completed
      ; file_watcher_tasks
      ; invalidation_events
      ; timers
      ; signals
      ; mutex
      ; cond
      ; pending_jobs
      ; worker_tasks_completed
      ; pending_worker_tasks
      ; stats
      ; got_event = false
      ; yield = None
      }

    let register_job_started q = q.pending_jobs <- q.pending_jobs + 1

    let register_worker_task_started q =
      q.pending_worker_tasks <- q.pending_worker_tasks + 1

    let add_event q f =
      Mutex.lock q.mutex;
      f q;
      if not q.got_event then (
        q.got_event <- true;
        Condition.signal q.cond
      );
      Mutex.unlock q.mutex

    let yield_if_there_are_pending_events q =
      if Config.inside_dune || not q.got_event then
        Fiber.return ()
      else
        match q.yield with
        | Some ivar -> Fiber.Ivar.read ivar
        | None ->
          let ivar = Fiber.Ivar.create () in
          q.yield <- Some ivar;
          Fiber.Ivar.read ivar

    module Event_source : sig
      type queue := t

      type t

      val signal : t

      val file_watcher_task : t

      val invalidation : t

      val jobs_completed : t

      val worker_tasks_completed : t

      val yield : t

      val timers : t

      val chain : t list -> t

      val run : t -> queue -> event option
    end = struct
      type queue = t

      type t = queue -> event option

      let run t q = t q

      let signal : t =
       fun q ->
        Option.map (Signal.Set.choose q.signals) ~f:(fun signal ->
            q.signals <- Signal.Set.remove q.signals signal;
            Signal signal)

      let file_watcher_task q =
        Option.map (Queue.pop q.file_watcher_tasks) ~f:(fun job ->
            File_watcher_task job)

      let invalidation q =
        match q.invalidation_events with
        | [] -> None
        | events -> (
          q.invalidation_events <- [];
          let terminated = ref false in
          let events =
            List.concat_map events ~f:(function
              | Filesystem_event Sync -> [ (Sync : build_input_change) ]
              | Invalidation invalidation ->
                [ (Invalidation invalidation : build_input_change) ]
              | Filesystem_event Watcher_terminated ->
                terminated := true;
                []
              | Filesystem_event (Fs_memo_event event) -> [ Fs_event event ]
              | Filesystem_event Queue_overflow ->
                [ Invalidation Memo.Invalidation.clear_caches ])
          in
          match !terminated with
          | true -> Some File_system_watcher_terminated
          | false ->
            Option.map (Nonempty_list.of_list events) ~f:(fun events ->
                Build_inputs_changed events))

      let jobs_completed q =
        Option.map (Queue.pop q.jobs_completed) ~f:(fun (job, proc_info) ->
            q.pending_jobs <- q.pending_jobs - 1;
            assert (q.pending_jobs >= 0);
            Job_completed (job, proc_info))

      let worker_tasks_completed q =
        Option.map (Queue.pop q.worker_tasks_completed) ~f:(fun fill ->
            q.pending_worker_tasks <- q.pending_worker_tasks - 1;
            Worker_task fill)

      let yield q =
        Option.map q.yield ~f:(fun ivar ->
            q.yield <- None;
            Yield ivar)

      let timers q =
        Option.map (Queue.pop q.timers) ~f:(fun timer -> Timer timer)

      let chain list q = List.find_map list ~f:(fun f -> f q)
    end

    let next q =
      Option.iter q.stats ~f:Dune_stats.record_gc_and_fd;
      Mutex.lock q.mutex;
      let rec loop () =
        match
          Event_source.(
            run
              (chain
                 (* Event sources are listed in priority order. Signals are the
                    highest priority to maximize responsiveness to Ctrl+C.
                    [file_watcher_task], [worker_tasks_completed] and
                    [invalidation] are used for reacting to user input, so their
                    latency is also important. [jobs_completed] and [yield] are
                    where the bulk of the work is done, so they are the lowest
                    priority to avoid starving other things. *)
                 [ signal
                 ; file_watcher_task
                 ; invalidation
                 ; worker_tasks_completed
                 ; jobs_completed
                 ; yield
                 ; timers
                 ]))
            q
        with
        | None -> wait ()
        | Some event -> event
      and wait () =
        q.got_event <- false;
        Condition.wait q.cond q.mutex;
        loop ()
      in
      let ev = loop () in
      Mutex.unlock q.mutex;
      ev

    let send_worker_task_completed q event =
      add_event q (fun q -> Queue.push q.worker_tasks_completed event)

    let send_invalidation_events q events =
      add_event q (fun q ->
          q.invalidation_events <- List.rev_append events q.invalidation_events)

    let send_file_watcher_events q files =
      send_invalidation_events q
        (List.map files ~f:(fun file : Invalidation_event.t ->
             Filesystem_event file))

    let send_invalidation_event q invalidation =
      send_invalidation_events q [ Invalidation invalidation ]

    let send_job_completed q job proc_info =
      add_event q (fun q -> Queue.push q.jobs_completed (job, proc_info))

    let send_signal q signal =
      add_event q (fun q -> q.signals <- Signal.Set.add q.signals signal)

    let send_file_watcher_task q job =
      add_event q (fun q -> Queue.push q.file_watcher_tasks job)

    let send_timers_completed q timers =
      add_event q (fun q ->
          Nonempty_list.to_list timers |> List.iter ~f:(Queue.push q.timers))

    let pending_jobs q = q.pending_jobs

    let pending_worker_tasks q = q.pending_worker_tasks
  end
end

module Process_watcher : sig
  (** Initialize the process watcher thread. *)
  type t

  val init : Event.Queue.t -> t

  (** Register a new running job. *)
  val register_job : t -> job -> unit

  val is_running : t -> Pid.t -> bool

  (** Send the following signal to all running processes. *)
  val killall : t -> int -> unit
end = struct
  type process_state =
    | Running of job
    | Zombie of Proc.Process_info.t

  (* This mutable table is safe: it does not interact with the state we track in
     the build system. *)
  type t =
    { mutex : Mutex.t
    ; something_is_running : Condition.t
    ; table : (Pid.t, process_state) Table.t
    ; events : Event.Queue.t
    ; mutable running_count : int
    }

  let is_running t pid =
    Mutex.lock t.mutex;
    let res = Table.mem t.table pid in
    Mutex.unlock t.mutex;
    res

  module Process_table : sig
    val add : t -> job -> unit

    val remove : t -> Proc.Process_info.t -> unit

    val running_count : t -> int

    val iter : t -> f:(job -> unit) -> unit
  end = struct
    let add t job =
      match Table.find t.table job.pid with
      | None ->
        Table.set t.table job.pid (Running job);
        t.running_count <- t.running_count + 1;
        if t.running_count = 1 then Condition.signal t.something_is_running
      | Some (Zombie proc_info) ->
        Table.remove t.table job.pid;
        Event.Queue.send_job_completed t.events job proc_info
      | Some (Running _) -> assert false

    let remove t (proc_info : Proc.Process_info.t) =
      match Table.find t.table proc_info.pid with
      | None -> Table.set t.table proc_info.pid (Zombie proc_info)
      | Some (Running job) ->
        t.running_count <- t.running_count - 1;
        Table.remove t.table proc_info.pid;
        Event.Queue.send_job_completed t.events job proc_info
      | Some (Zombie _) -> assert false

    let iter t ~f =
      Table.iter t.table ~f:(fun data ->
          match data with
          | Running job -> f job
          | Zombie _ -> ())

    let running_count t = t.running_count
  end

  let register_job t job =
    Event.Queue.register_job_started t.events;
    Mutex.lock t.mutex;
    Process_table.add t job;
    Mutex.unlock t.mutex

  let killall t signal =
    Mutex.lock t.mutex;
    Process_table.iter t ~f:(fun job ->
        try Unix.kill (Pid.to_int job.pid) signal with
        | Unix.Unix_error _ -> ());
    Mutex.unlock t.mutex

  exception Finished of Proc.Process_info.t

  let wait_nonblocking_win32 t =
    try
      Process_table.iter t ~f:(fun job ->
          let pid, status = Unix.waitpid [ WNOHANG ] (Pid.to_int job.pid) in
          if pid <> 0 then
            let now = Unix.gettimeofday () in
            let info : Proc.Process_info.t =
              { pid = Pid.of_int pid
              ; status
              ; end_time = now
              ; resource_usage = None
              }
            in
            raise_notrace (Finished info));
      false
    with
    | Finished proc_info ->
      (* We need to do the [Unix.waitpid] and remove the process while holding
         the lock, otherwise the pid might be reused in between. *)
      Process_table.remove t proc_info;
      true

  let wait_win32 t =
    while not (wait_nonblocking_win32 t) do
      Mutex.unlock t.mutex;
      Thread.delay 0.001;
      Mutex.lock t.mutex
    done

  let wait_unix t =
    Mutex.unlock t.mutex;
    let proc_info = Proc.wait [] in
    Mutex.lock t.mutex;
    Process_table.remove t proc_info

  let wait =
    if Sys.win32 then
      wait_win32
    else
      wait_unix

  let run t =
    Mutex.lock t.mutex;
    while true do
      while Process_table.running_count t = 0 do
        Condition.wait t.something_is_running t.mutex
      done;
      wait t
    done

  let init events =
    let t =
      { mutex = Mutex.create ()
      ; something_is_running = Condition.create ()
      ; table = Table.create (module Pid) 128
      ; events
      ; running_count = 0
      }
    in
    ignore (Thread.create run t : Thread.t);
    t
end

module Signal_watcher : sig
  val init : Event.Queue.t -> unit
end = struct
  let signos = List.map Signal.all ~f:Signal.to_int

  let warning =
    {|

**************************************************************
* Press Control+C again quickly to perform an emergency exit *
**************************************************************

|}

  external sys_exit : int -> _ = "caml_sys_exit"

  let signal_waiter () =
    if Sys.win32 then (
      let r, w = Unix.pipe ~cloexec:true () in
      let buf = Bytes.create 1 in
      Sys.set_signal Sys.sigint
        (Signal_handle (fun _ -> assert (Unix.write w buf 0 1 = 1)));
      Staged.stage (fun () ->
          assert (Unix.read r buf 0 1 = 1);
          Signal.Int)
    ) else
      Staged.stage (fun () ->
          Thread.wait_signal signos |> Signal.of_int |> Option.value_exn)

  let run q =
    let last_exit_signals = Queue.create () in
    let wait_signal = Staged.unstage (signal_waiter ()) in
    while true do
      let signal = wait_signal () in
      Event.Queue.send_signal q signal;
      match signal with
      | Int
      | Quit
      | Term ->
        let now = Unix.gettimeofday () in
        Queue.push last_exit_signals now;
        (* Discard old signals *)
        while
          Queue.length last_exit_signals >= 0
          && now -. Queue.peek_exn last_exit_signals > 1.
        do
          ignore (Queue.pop_exn last_exit_signals : float)
        done;
        let n = Queue.length last_exit_signals in
        if n = 2 then prerr_endline warning;
        if n = 3 then sys_exit 1
    done

  let init q = ignore (Thread.create run q : Thread.t)
end

type status =
  | (* Ready to start the next build. Waiting for a signal from the user, the
       test harness, or the polling loop. The payload is the collection of
       filesystem events. *)
      Standing_by of
      Memo.Invalidation.t
  | (* Waiting for file changes to start a new a build *)
      Waiting_for_file_changes of
      Memo.Invalidation.t Fiber.Ivar.t
  | (* Waiting for the propagation of inotify events to finish before starting a
       build. *)
      Waiting_for_inotify_sync of
      Memo.Invalidation.t * unit Fiber.Ivar.t
  | (* Running a build *)
      Building
  | (* Cancellation requested. Build jobs are immediately rejected in this
       state *)
      Restarting_build of
      Memo.Invalidation.t
  | (* Shut down requested. No new new builds will start *)
      Shutting_down

module Handler = struct
  module Event = struct
    type build_result =
      | Success
      | Failure

    type t =
      | Tick
      | Source_files_changed
      | Build_interrupted
      | Build_finish of build_result
  end

  type t = Config.t -> Event.t -> unit
end

module Alarm_clock : sig
  type t

  val create : Event.Queue.t -> frequency:float -> t

  type alarm

  val await : alarm -> [ `Finished | `Cancelled ] Fiber.t

  val cancel : t -> alarm -> unit

  val sleep : t -> float -> alarm

  val close : t -> unit
end = struct
  type alarm = [ `Finished | `Cancelled ] Fiber.Ivar.t

  type t =
    { events : Event.Queue.t
    ; mutex : Mutex.t
    ; frequency : float
    ; mutable alarms : (float * [ `Finished | `Cancelled ] Fiber.Ivar.t) list
    ; mutable active : bool
    }

  let await = Fiber.Ivar.read

  let cancel t alarm =
    Mutex.lock t.mutex;
    let found = ref false in
    t.alarms <-
      List.filter t.alarms ~f:(fun (_, alarm') ->
          let eq = alarm' == alarm in
          if eq then found := true;
          not eq);
    Mutex.unlock t.mutex;
    if !found then
      Event.Queue.send_timers_completed t.events
        [ Fiber.Fill (alarm, `Cancelled) ]

  let polling_loop t () =
    let rec loop () =
      match t.active with
      | false -> ()
      | true ->
        let now = Unix.gettimeofday () in
        let expired, active =
          List.partition_map t.alarms ~f:(fun (expiration, ivar) ->
              if now > expiration then
                Left (Fiber.Fill (ivar, `Finished))
              else
                Right (expiration, ivar))
        in
        t.alarms <- active;
        Mutex.unlock t.mutex;
        (match Nonempty_list.of_list expired with
        | None -> ()
        | Some expired -> Event.Queue.send_timers_completed t.events expired);
        Thread.delay t.frequency;
        Mutex.lock t.mutex;
        loop ()
    in
    Mutex.lock t.mutex;
    loop ();
    t.alarms <- [];
    Mutex.unlock t.mutex

  let create events ~frequency =
    let t =
      { events; active = true; alarms = []; frequency; mutex = Mutex.create () }
    in
    Thread.spawn (polling_loop t);
    t

  let sleep t duration =
    Mutex.lock t.mutex;
    let ivar = Fiber.Ivar.create () in
    if not t.active then (
      Mutex.unlock t.mutex;
      Code_error.raise "cannot schedule timers after close" []
    );
    t.alarms <- (duration +. Unix.gettimeofday (), ivar) :: t.alarms;
    Mutex.unlock t.mutex;
    ivar

  let close t =
    Mutex.lock t.mutex;
    t.active <- false;
    Mutex.unlock t.mutex
end

type t =
  { config : Config.t
  ; alarm_clock : Alarm_clock.t Lazy.t
  ; mutable status : status
  ; handler : Handler.t
  ; job_throttle : Fiber.Throttle.t
  ; events : Event.Queue.t
  ; process_watcher : Process_watcher.t
  ; file_watcher : Dune_file_watcher.t option
  }

let t : t Fiber.Var.t = Fiber.Var.create ()

let set x f = Fiber.Var.set t x f

let t_opt () = Fiber.Var.get t

let t () = Fiber.Var.get_exn t

let running_jobs_count t = Event.Queue.pending_jobs t.events

let yield_if_there_are_pending_events () =
  t_opt () >>= function
  | None -> Fiber.return ()
  | Some t -> Event.Queue.yield_if_there_are_pending_events t.events

let () =
  Memo.yield_if_there_are_pending_events := yield_if_there_are_pending_events

let ignore_for_watch path =
  let+ t = t () in
  match t.file_watcher with
  | None -> ()
  | Some file_watcher ->
    assert (Path.is_in_source_tree path);
    Dune_file_watcher.ignore_next_file_change_event file_watcher path

exception Build_cancelled

let with_job_slot f =
  let* t = t () in
  let raise_if_cancelled () =
    match t.status with
    | Restarting_build _
    | Shutting_down ->
      raise (Memo.Non_reproducible Build_cancelled)
    | Building -> ()
    | Waiting_for_file_changes _
    | Waiting_for_inotify_sync _
    | Standing_by _ ->
      (* At this stage, we're not running a build, so we shouldn't be running
         tasks here. *)
      assert false
  in
  Fiber.Throttle.run t.job_throttle ~f:(fun () ->
      raise_if_cancelled ();
      Fiber.collect_errors (fun () -> f t.config) >>= function
      | Ok res -> Fiber.return res
      | Error exns ->
        raise_if_cancelled ();
        Fiber.reraise_all exns)

(* We use this version privately in this module whenever we can pass the
   scheduler explicitly *)
let wait_for_process t pid =
  let ivar = Fiber.Ivar.create () in
  Process_watcher.register_job t.process_watcher { pid; ivar };
  Fiber.Ivar.read ivar

let global = ref None

let got_signal signal =
  if !Log.verbose then
    Log.info [ Pp.textf "Got signal %s, exiting." (Signal.name signal) ]

let filesystem_watcher_terminated () =
  Log.info [ Pp.textf "Filesystem watcher terminated, exiting." ]

type saw_signal =
  | Ok
  | Got_signal

let kill_and_wait_for_all_processes t =
  Process_watcher.killall t.process_watcher Sys.sigkill;
  let saw_signal = ref Ok in
  while Event.Queue.pending_jobs t.events > 0 do
    match Event.Queue.next t.events with
    | Signal signal ->
      got_signal signal;
      saw_signal := Got_signal
    | _ -> ()
  done;
  !saw_signal

let prepare (config : Config.t) ~(handler : Handler.t) =
  let events = Event.Queue.create config.stats in
  (* We return the scheduler in chunks to resolve the dependency cycle
     (scheduler wants to know the file_watcher, file_watcher wants to send
     events to scheduler) *)
  ( events
  , fun ~file_watcher ->
      (* The signal watcher must be initialized first so that signals are
         blocked in all threads. *)
      Signal_watcher.init events;
      let process_watcher = Process_watcher.init events in
      let t =
        { status =
            (* Slightly weird initialization happening here: for polling mode we
               initialize in "Building" state, immediately switch to Standing_by
               and then back to "Building". It would make more sense to start in
               "Stand_by" from the start. We can't "just" switch the initial
               value here because then the non-polling mode would run in
               "Standing_by" mode, which is even weirder. *)
            Building
        ; job_throttle = Fiber.Throttle.create config.concurrency
        ; process_watcher
        ; events
        ; config
        ; handler
        ; file_watcher
        ; alarm_clock = lazy (Alarm_clock.create events ~frequency:0.1)
        }
      in
      global := Some t;
      t )

module Run_once : sig
  type run_error =
    | Already_reported
    | Exn of Exn_with_backtrace.t

  (** Run the build and clean up after it (kill any stray processes etc). *)
  val run_and_cleanup : t -> (unit -> 'a Fiber.t) -> ('a, run_error) Result.t
end = struct
  type run_error =
    | Already_reported
    | Exn of Exn_with_backtrace.t

  exception Abort of run_error

  let handle_invalidation_events events =
    let handle_event event =
      match (event : Event.build_input_change) with
      | Invalidation invalidation -> invalidation
      | Fs_event event -> Fs_memo.handle_fs_event event
      | Sync -> Memo.Invalidation.empty
    in
    let invalidation =
      let events = Nonempty_list.to_list events in
      List.fold_left events ~init:Memo.Invalidation.empty ~f:(fun acc event ->
          Memo.Invalidation.combine acc (handle_event event))
    in
    match !Memo.incremental_mode_enabled with
    | true -> invalidation
    | false ->
      (* In this mode, we do not assume that all file system dependencies are
         declared correctly and therefore conservatively require a rebuild.

         The fact that the [events] list is non-empty justifies clearing the
         caches. *)
      let (_ : _ Nonempty_list.t) = events in
      Memo.Invalidation.clear_caches

  (** This function is the heart of the scheduler. It makes progress in
      executing fibers by doing the following:

      - notifying completed jobs
      - starting cancellations
      - terminating the scheduler on signals *)
  let rec iter (t : t) =
    t.handler t.config Tick;
    match Event.Queue.next t.events with
    | Job_completed (job, proc_info) -> Fiber.Fill (job.ivar, proc_info)
    | File_watcher_task job ->
      let events = job () in
      Event.Queue.send_file_watcher_events t.events events;
      iter t
    | Build_inputs_changed events -> (
      let invalidation =
        (handle_invalidation_events events : Memo.Invalidation.t)
      in
      let have_sync =
        List.exists (Nonempty_list.to_list events) ~f:(function
          | (Sync : Event.build_input_change) -> true
          | _ -> false)
      in
      match Memo.Invalidation.is_empty invalidation && not have_sync with
      | true -> iter t (* Ignore the event *)
      | false -> (
        match t.status with
        | Shutting_down -> iter t
        | Restarting_build prev_invalidation ->
          t.status <-
            Restarting_build
              (Memo.Invalidation.combine prev_invalidation invalidation);
          (* We're already cancelling build, so file change events don't
             matter *)
          iter t
        | Standing_by prev_invalidation ->
          t.status <-
            Standing_by
              (Memo.Invalidation.combine prev_invalidation invalidation);
          iter t
        | Building ->
          t.handler t.config Build_interrupted;
          t.status <- Restarting_build invalidation;
          Process_watcher.killall t.process_watcher Sys.sigkill;
          iter t
        | Waiting_for_file_changes ivar -> Fill (ivar, invalidation)
        | Waiting_for_inotify_sync (prev_invalidation, ivar) ->
          let invalidation =
            Memo.Invalidation.combine prev_invalidation invalidation
          in
          if have_sync then (
            t.status <- Standing_by invalidation;
            Fill (ivar, ())
          ) else (
            t.status <- Waiting_for_inotify_sync (invalidation, ivar);
            iter t
          )))
    | Timer fill
    | Worker_task fill ->
      fill
    | File_system_watcher_terminated ->
      filesystem_watcher_terminated ();
      raise (Abort Already_reported)
    | Signal signal ->
      got_signal signal;
      raise (Abort Already_reported)
    | Yield ivar -> Fill (ivar, ())

  let run t f : _ result =
    let fiber =
      set t (fun () ->
          Fiber.map_reduce_errors
            (module Monoid.Unit)
            f
            ~on_error:(fun e ->
              Dune_util.Report_error.report e;
              Fiber.return ()))
    in
    match Fiber.run fiber ~iter:(fun () -> iter t) with
    | Ok res ->
      assert (Event.Queue.pending_jobs t.events = 0);
      assert (Event.Queue.pending_worker_tasks t.events = 0);
      Ok res
    | Error () -> Error Already_reported
    | exception Abort err -> Error err
    | exception exn -> Error (Exn (Exn_with_backtrace.capture exn))

  let run_and_cleanup t f =
    let res = run t f in
    Console.Status_line.set_constant None;
    match kill_and_wait_for_all_processes t with
    | Got_signal -> Error Already_reported
    | Ok -> res
end

module Worker = struct
  type t =
    { worker : Thread_worker.t
    ; events : Event.Queue.t
    }

  let stop t = Thread_worker.stop t.worker

  let create () =
    let worker = Thread_worker.create ~spawn_thread:Thread.spawn in
    let+ scheduler = t () in
    { worker; events = scheduler.events }

  let task (t : t) ~f =
    let ivar = Fiber.Ivar.create () in
    let f () =
      let res = Exn_with_backtrace.try_with f in
      Event.Queue.send_worker_task_completed t.events (Fiber.Fill (ivar, res))
    in
    match Thread_worker.add_work t.worker ~f with
    | Error `Stopped -> Fiber.return (Error `Stopped)
    | Ok () -> (
      Event.Queue.register_worker_task_started t.events;
      let+ res = Fiber.Ivar.read ivar in
      match res with
      | Error exn -> Error (`Exn exn)
      | Ok e -> Ok e)

  let task_exn t ~f =
    let+ res = task t ~f in
    match res with
    | Ok a -> a
    | Error `Stopped ->
      Code_error.raise "Scheduler.Worker.task_exn: worker stopped" []
    | Error (`Exn e) -> Exn_with_backtrace.reraise e
end

module Run = struct
  exception Build_cancelled = Build_cancelled

  type file_watcher =
    | Automatic
    | No_watcher

  module Event_queue = Event.Queue
  module Event = Handler.Event

  module Build_outcome = struct
    type t =
      | Shutdown
      | Cancelled_due_to_file_changes
      | Finished of (unit, [ `Already_reported ]) Result.t
  end

  let poll_iter t step =
    (match t.status with
    | Standing_by invalidations ->
      if false then
        Console.print
          [ Pp.text "Invalidating:"
          ; Dyn.pp (Memo.Invalidation.to_dyn invalidations)
          ];
      Memo.reset invalidations
    | _ ->
      Code_error.raise "[poll_iter]: expected the build status [Standing_by]" []);
    t.status <- Building;
    let+ res = step in
    match t.status with
    | Waiting_for_file_changes _
    | Waiting_for_inotify_sync _
    | Standing_by _ ->
      (* We just finished a build, so there's no way this was set *)
      assert false
    | Shutting_down -> Build_outcome.Shutdown
    | Restarting_build invalidations ->
      t.status <- Standing_by invalidations;
      Build_outcome.Cancelled_due_to_file_changes
    | Building ->
      let build_result : Handler.Event.build_result =
        match res with
        | Error `Already_reported -> Failure
        | Ok _ -> Success
      in
      t.handler t.config (Build_finish build_result);
      t.status <- Standing_by Memo.Invalidation.empty;
      Build_outcome.Finished res

  type handle_outcome_result =
    | Shutdown
    | Proceed

  type step = (unit, [ `Already_reported ]) Result.t Fiber.t

  let poll_gen ~get_build_request =
    let* t = t () in
    (match t.status with
    | Building -> t.status <- Standing_by Memo.Invalidation.empty
    | _ -> assert false);
    let rec loop () =
      let* (build_request : step), handle_outcome = get_build_request t in
      let* res = poll_iter t build_request in
      let* next = handle_outcome res in
      match next with
      | Shutdown -> Fiber.return ()
      | Proceed -> loop ()
    in
    loop ()

  let poll step =
    poll_gen ~get_build_request:(fun (t : t) ->
        let handle_outcome (outcome : Build_outcome.t) =
          match outcome with
          | Shutdown -> Fiber.return Shutdown
          | Cancelled_due_to_file_changes -> Fiber.return Proceed
          | Finished _res ->
            let ivar = Fiber.Ivar.create () in
            t.status <- Waiting_for_file_changes ivar;
            let* invalidations = Fiber.Ivar.read ivar in
            t.status <- Standing_by invalidations;
            t.handler t.config Source_files_changed;
            Fiber.return Proceed
        in
        Fiber.return (step, handle_outcome))

  let wait_for_inotify_sync t =
    let ivar = Fiber.Ivar.create () in
    match t.status with
    | Standing_by invalidation ->
      t.status <- Waiting_for_inotify_sync (invalidation, ivar);
      Fiber.Ivar.read ivar
    | _ -> assert false

  let do_inotify_sync t =
    Dune_file_watcher.emit_sync ();
    Console.print [ Pp.text "waiting for inotify sync" ];
    let+ () = wait_for_inotify_sync t in
    Console.print [ Pp.text "waited for inotify sync" ]

  module Build_outcome_for_rpc = struct
    type t =
      | Success
      | Failure
  end

  let poll_passive ~get_build_request =
    poll_gen ~get_build_request:(fun t ->
        let* request, response_ivar = get_build_request in
        let+ () = do_inotify_sync t in
        let handle_outcome (res : Build_outcome.t) =
          let+ () =
            Fiber.Ivar.fill response_ivar
              (match res with
              | Finished (Ok _) -> Build_outcome_for_rpc.Success
              | Finished (Error _)
              | Cancelled_due_to_file_changes
              | Shutdown ->
                Build_outcome_for_rpc.Failure)
          in
          Proceed
        in
        (request, handle_outcome))

  exception Shutdown_requested

  let go config ?(file_watcher = No_watcher)
      ~(on_event : Config.t -> Handler.Event.t -> unit) run =
    let events, prepare = prepare config ~handler:on_event in
    let file_watcher =
      match file_watcher with
      | No_watcher -> None
      | Automatic ->
        Some
          (Dune_file_watcher.create_default
             ~scheduler:
               { spawn_thread = Thread.spawn
               ; thread_safe_send_emit_events_job =
                   (fun job -> Event_queue.send_file_watcher_task events job)
               })
    in
    let t = prepare ~file_watcher in
    let initial_invalidation = Fs_memo.init ~dune_file_watcher:file_watcher in
    Memo.reset initial_invalidation;
    let result =
      match Run_once.run_and_cleanup t run with
      | Ok a -> Result.Ok a
      | Error Already_reported ->
        let exn =
          if t.status = Shutting_down then
            Shutdown_requested
          else
            Dune_util.Report_error.Already_reported
        in
        Error (exn, None)
      | Error (Exn exn_with_bt) ->
        Error (exn_with_bt.exn, Some exn_with_bt.backtrace)
    in
    Option.iter file_watcher ~f:(fun watcher ->
        match Dune_file_watcher.shutdown watcher with
        | `Kill pid -> ignore (wait_for_process t pid : _ Fiber.t)
        | `No_op -> ());
    ignore (kill_and_wait_for_all_processes t : saw_signal);
    if Lazy.is_val t.alarm_clock then
      Alarm_clock.close (Lazy.force t.alarm_clock);
    match result with
    | Ok a -> a
    | Error (exn, None) -> Exn.raise exn
    | Error (exn, Some bt) -> Exn.raise_with_backtrace exn bt
end

let shutdown () =
  let+ t = t () in
  t.status <- Shutting_down;
  Event.Queue.send_signal t.events Quit

let inject_memo_invalidation invalidation =
  let* t = t () in
  Event.Queue.send_invalidation_event t.events invalidation;
  Fiber.return ()

let wait_for_process_with_timeout t pid ~timeout =
  Fiber.of_thunk (fun () ->
      let sleep = Alarm_clock.sleep (Lazy.force t.alarm_clock) timeout in
      Fiber.fork_and_join_unit
        (fun () ->
          let+ res = Alarm_clock.await sleep in
          if res = `Finished && Process_watcher.is_running t.process_watcher pid
          then
            Unix.kill (Pid.to_int pid) Sys.sigkill)
        (fun () ->
          let+ res = wait_for_process t pid in
          Alarm_clock.cancel (Lazy.force t.alarm_clock) sleep;
          res))

let wait_for_process ?timeout pid =
  let* t = t () in
  match timeout with
  | None -> wait_for_process t pid
  | Some timeout -> wait_for_process_with_timeout t pid ~timeout

let sleep duration =
  let* t = t () in
  let alarm_clock = Lazy.force t.alarm_clock in
  let+ res = Alarm_clock.await (Alarm_clock.sleep alarm_clock duration) in
  match res with
  | `Finished -> ()
  | `Cancelled ->
    (* cancellation mechanism isn't exposed to the user *)
    assert false
