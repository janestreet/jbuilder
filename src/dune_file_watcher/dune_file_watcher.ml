open! Stdune
module Inotify_lib = Async_inotify_for_dune.Async_inotify

let inotify_event_paths (event : Inotify_lib.Event.t) =
  match event with
  | Created path
  | Unlinked path
  | Modified path
  | Moved (Away path)
  | Moved (Into path) ->
    [ path ]
  | Moved (Move (from, to_)) -> [ from; to_ ]
  | Queue_overflow -> []

type kind =
  | Coarse of { wait_for_watches_established : unit -> unit }
  | Fine of { inotify : Inotify_lib.t }

type t =
  { shutdown : [ `Kill of Pid.t | `No_op ]
  ; kind : kind
        (* CR-someday amokhov: The way we handle "ignored files" using this
           mutable table is fragile and also wrong. We use [ignored_files] for
           the [(mode promote)] feature: if a file is promoted, we call
           [ignore_next_file_change_event] so that the upcoming file-change
           event does not invalidate the current build. However, instead of
           ignoring the events, we should merely postpone them and restart the
           build to take the promoted files into account if need be. *)
        (* The [ignored_files] table should be accessed in the scheduler
           thread. *)
  ; ignored_files : (string, unit) Table.t
  }

module Fs_memo_event = struct
  type kind =
    | Created
    | Deleted
    | File_changed
    | Unknown  (** Treated conservatively as any possible event. *)

  type t =
    { path : Path.t
    ; kind : kind
    }

  let create ~kind ~path =
    if Path.is_in_build_dir path then
      Code_error.raise "Fs_memo.Event.create called on a build path" [];
    { path; kind }
end

module Event = struct
  type t =
    | Fs_memo_event of Fs_memo_event.t
    | Queue_overflow
    | Sync
    | Watcher_terminated
end

(* [process_inotify_event] needs to run in the scheduler thread because it
   accesses [t.ignored_files]. *)
let process_inotify_event ~ignored_files
    (event : Async_inotify_for_dune.Async_inotify.Event.t) : Event.t list =
  let should_ignore =
    List.exists (inotify_event_paths event) ~f:(fun path ->
        let path = Path.of_string path in
        let abs_path = Path.to_absolute_filename path in
        if Table.mem ignored_files abs_path then (
          (* only use ignored record once *)
          Table.remove ignored_files abs_path;
          true
        ) else
          false)
  in
  if should_ignore then
    []
  else
    match event with
    | Created path ->
      let path = Path.of_string path in
      [ Fs_memo_event (Fs_memo_event.create ~kind:Created ~path) ]
    | Unlinked path ->
      let path = Path.of_string path in
      [ Fs_memo_event (Fs_memo_event.create ~kind:Deleted ~path) ]
    | Modified path ->
      let path = Path.of_string path in
      [ Fs_memo_event (Fs_memo_event.create ~kind:File_changed ~path) ]
    | Moved move -> (
      match move with
      | Away path ->
        let path = Path.of_string path in
        [ Fs_memo_event (Fs_memo_event.create ~kind:Deleted ~path) ]
      | Into path ->
        let path = Path.of_string path in
        [ Fs_memo_event (Fs_memo_event.create ~kind:Created ~path) ]
      | Move (from, to_) ->
        let from = Path.of_string from in
        let to_ = Path.of_string to_ in
        [ Fs_memo_event (Fs_memo_event.create ~kind:Deleted ~path:from)
        ; Fs_memo_event (Fs_memo_event.create ~kind:Created ~path:to_)
        ])
    | Queue_overflow -> [ Event.Queue_overflow ]

module Scheduler = struct
  type t =
    { spawn_thread : (unit -> unit) -> unit
    ; thread_safe_send_emit_events_job : (unit -> Event.t list) -> unit
    }
end

let shutdown t = t.shutdown

let buffer_capacity = 65536

(* Fixed-size buffer for reading line-by-line from file descriptors. Bug:
   deadlocks if there's a line longer than the capacity of the buffer. TODO: use
   In_channel? *)
module Buffer = struct
  type buffer =
    { data : Bytes.t
    ; mutable size : int
    }

  let create ~capacity = { data = Bytes.create capacity; size = 0 }

  let read_lines buffer fd =
    let len =
      Unix.read fd buffer.data buffer.size (buffer_capacity - buffer.size)
    in
    buffer.size <- buffer.size + len;
    if len = 0 then
      `End_of_file (Bytes.sub_string buffer.data ~pos:0 ~len:buffer.size)
    else
      `Ok
        (let lines = ref [] in
         let line_start = ref 0 in
         for i = 0 to buffer.size - 1 do
           let c = Bytes.get buffer.data i in
           if c = '\n' || c = '\r' then (
             (if !line_start < i then
               let line =
                 Bytes.sub_string buffer.data ~pos:!line_start
                   ~len:(i - !line_start)
               in
               lines := line :: !lines);
             line_start := i + 1
           )
         done;
         buffer.size <- buffer.size - !line_start;
         Bytes.blit ~src:buffer.data ~src_pos:!line_start ~dst:buffer.data
           ~dst_pos:0 ~len:buffer.size;
         List.rev !lines)
end

module Inotifywait = struct
  let wait_for_watches_established stderr =
    let buffer = Buffer.create ~capacity:65536 in
    let rec loop () =
      match Buffer.read_lines buffer stderr with
      | `End_of_file _last_line -> `Error
      | `Ok lines ->
        if List.exists lines ~f:(String.equal "Watches established.") then
          `Established
        else
          loop ()
    in
    loop ()

  let parse_message s =
    match String.drop_prefix ~prefix:"e:" s with
    | None -> Error "invalid message (prefix missing)"
    | Some event -> (
      match String.lsplit2 ~on:':' event with
      | Some (_kind, path) -> Ok path
      | None -> Error "invalid message (event type missing)")
end

let special_file_for_inotify_sync =
  let path = lazy (Path.Build.relative Path.Build.root "dune-inotify-sync") in
  fun () -> Lazy.force path

let command ~root ~backend =
  let exclude_patterns =
    [ {|/_opam|}
    ; {|/_esy|}
    ; {|/\..+|}
    ; {|~$|}
    ; {|/#[^#]*#$|}
    ; {|4913|} (* https://github.com/neovim/neovim/issues/3460 *)
    ]
  in
  let exclude_paths =
    (* These paths should already exist on the filesystem when the watches are
       initially set up, otherwise the @<path> has no effect for inotifywait. If
       the file is deleted and re-created then "exclusion" is lost. This is why
       we're not including "_opam" and "_esy" in this list, in case they are
       created when dune is already running. *)
    (* these paths are used as patterns for fswatch, so they better not contain
       any regex-special characters *)
    [ "_build" ]
  in
  let root = Path.to_string root in
  let inotify_special_path =
    Path.Build.to_string (special_file_for_inotify_sync ())
  in
  match backend with
  | `Inotifywait inotifywait ->
    (* On Linux, use inotifywait. *)
    let excludes = String.concat ~sep:"|" exclude_patterns in
    ( inotifywait
    , List.concat
        [ [ "-r"; root ]
          (* excluding with "@" is more efficient that using --exclude because
             it avoids creating inotify watches altogether, while --exclude
             merely filters the events after they are generated *)
        ; List.map exclude_paths ~f:(fun path ->
              "@" ^ Filename.concat root path)
        ; [ inotify_special_path ]
        ; [ "--exclude"; excludes ]
        ; [ "-e"; "close_write" ]
        ; [ "-e"; "delete" ]
        ; [ "--format"; "e:%e:%w%f" ]
        ; [ "-m" ]
        ]
    , Inotifywait.parse_message
    , Some Inotifywait.wait_for_watches_established )
  | `Fswatch fswatch ->
    (* On all other platforms, try to use fswatch. fswatch's event filtering is
       not reliable (at least on Linux), so don't try to use it, instead act on
       all events. *)
    let excludes =
      List.concat_map
        (exclude_patterns @ List.map exclude_paths ~f:(fun p -> "/" ^ p))
        ~f:(fun x -> [ "--exclude"; x ])
    in
    ( fswatch
    , [ "-r"
      ; root
      ; (* If [inotify_special_path] is not passed here, then the [--exclude
           _build] makes fswatch not descend into [_build], which means it never
           even discovers that [inotify_special_path] exists. This is despite
           the fact that [--include] appears before. *)
        inotify_special_path
      ; "--event"
      ; "Created"
      ; "--event"
      ; "Updated"
      ; "--event"
      ; "Removed"
      ]
      @ [ "--include"; inotify_special_path ]
      @ excludes
    , (fun s -> Ok s)
    , None )

let select_watcher_backend ~use_inotify_lib =
  let try_fswatch () =
    Option.map
      (Bin.which ~path:(Env.path Env.initial) "fswatch")
      ~f:(fun fswatch -> `Fswatch fswatch)
  in
  let try_inotifywait () =
    Option.map
      (Bin.which ~path:(Env.path Env.initial) "inotifywait")
      ~f:(fun inotifywait -> `Inotifywait inotifywait)
  in
  let error str = User_error.raise [ Pp.text str ] in
  match Sys.linux with
  | false -> (
    match try_fswatch () with
    | Some res -> res
    | None -> error "Please install fswatch to enable watch mode.")
  | true -> (
    if use_inotify_lib then (
      assert (Ocaml_inotify.Inotify.supported_by_the_os ());
      `Inotify_lib
    ) else
      match try_inotifywait () with
      | Some res -> res
      | None -> (
        match try_fswatch () with
        | Some res -> res
        | None ->
          User_error.raise
            [ Pp.text
                "Please install inotifywait to enable watch mode. If \
                 inotifywait is unavailable, fswatch may also be used but will \
                 result in a worse experience."
            ]))

let emit_sync () =
  Io.write_file (Path.build (special_file_for_inotify_sync ())) "z"

let prepare_sync () =
  Path.mkdir_p (Path.parent_exn (Path.build (special_file_for_inotify_sync ())));
  emit_sync ()

let spawn_external_watcher ~root ~backend =
  prepare_sync ();
  let prog, args, parse_line, wait_for_start = command ~root ~backend in
  let prog = Path.to_absolute_filename prog in
  let argv = prog :: args in
  let r_stdout, w_stdout = Unix.pipe () in
  let stderr, wait =
    match wait_for_start with
    | None -> (None, fun () -> ())
    | Some wait -> (
      let r_stderr, w_stderr = Unix.pipe () in
      ( Some w_stderr
      , fun () ->
          match wait r_stderr with
          | `Error -> failwith "error waiting for watches to be established"
          | `Established -> () ))
  in
  let pid = Spawn.spawn () ~prog ~argv ~stdout:w_stdout ?stderr |> Pid.of_int in
  Unix.close w_stdout;
  Option.iter stderr ~f:Unix.close;
  ((r_stdout, parse_line, wait), pid)

let create_inotifylib_watcher ~ignored_files ~(scheduler : Scheduler.t) =
  let special_file_for_inotify_sync = special_file_for_inotify_sync () in
  Inotify_lib.create ~spawn_thread:scheduler.spawn_thread
    ~modify_event_selector:`Closed_writable_fd
    ~send_emit_events_job_to_scheduler:(fun f ->
      scheduler.thread_safe_send_emit_events_job (fun () ->
          let events = f () in
          List.concat_map events ~f:(fun event ->
              match (event : Inotify_lib.Event.t) with
              | Modified path
                when Path.equal (Path.of_string path)
                       (Path.build special_file_for_inotify_sync) ->
                [ Event.Sync ]
              | event -> process_inotify_event ~ignored_files event)))
    ~log_error:(fun error -> Console.print [ Pp.text error ])

let special_file_for_inotify_sync_absolute =
  lazy
    (Path.to_absolute_filename (Path.build (special_file_for_inotify_sync ())))

let is_special_file_for_inotify_sync (path : Path.t) =
  match path with
  | In_source_tree _ -> false
  | External _ ->
    String.equal (Path.to_string path)
      (Lazy.force special_file_for_inotify_sync_absolute)
  | In_build_dir build_path ->
    Path.Build.( = ) build_path (special_file_for_inotify_sync ())

let create_no_buffering ~(scheduler : Scheduler.t) ~root ~backend =
  let ignored_files = Table.create (module String) 64 in
  let (pipe, parse_line, wait), pid = spawn_external_watcher ~root ~backend in
  let worker_thread pipe =
    let buffer = Buffer.create ~capacity:buffer_capacity in
    while true do
      (* the job must run on the scheduler thread because it accesses
         [ignored_files] *)
      let job =
        match Buffer.read_lines buffer pipe with
        | `End_of_file _remaining -> fun () -> [ Event.Watcher_terminated ]
        | `Ok lines ->
          fun () ->
            List.concat_map lines ~f:(fun line ->
                match parse_line line with
                | Error s -> failwith s
                | Ok path_s ->
                  let path = Path.of_string path_s in
                  if is_special_file_for_inotify_sync path then
                    [ Event.Sync ]
                  else
                    let abs_path = Path.to_absolute_filename path in
                    if Table.mem ignored_files abs_path then (
                      (* only use ignored record once *)
                      Table.remove ignored_files abs_path;
                      []
                    ) else
                      [ Fs_memo_event
                          (Fs_memo_event.create ~kind:File_changed ~path)
                      ])
      in
      scheduler.thread_safe_send_emit_events_job job
    done
  in
  scheduler.spawn_thread (fun () -> worker_thread pipe);
  { shutdown = `Kill pid
  ; kind = Coarse { wait_for_watches_established = wait }
  ; ignored_files
  }

let with_buffering ~create ~(scheduler : Scheduler.t) ~debounce_interval =
  let jobs = ref [] in
  let event_mtx = Mutex.create () in
  let event_cv = Condition.create () in
  let res =
    let thread_safe_send_emit_events_job job =
      Mutex.lock event_mtx;
      jobs := job :: !jobs;
      Condition.signal event_cv;
      Mutex.unlock event_mtx
    in
    let scheduler = { scheduler with thread_safe_send_emit_events_job } in
    create ~scheduler
  in
  (* The buffer thread is used to avoid flooding the main thread with file
     changes events when a lot of file changes are reported at once. In
     particular, this avoids restarting the build over and over in a short
     period of time when many events are reported at once.

     It works as follow:

     - when the first event is received, send it to the main thread immediately
     so that we get a fast response time

     - after the first event is received, buffer subsequent events for
     [debounce_interval] *)
  let rec buffer_thread () =
    Mutex.lock event_mtx;
    while List.is_empty !jobs do
      Condition.wait event_cv event_mtx
    done;
    let jobs_batch = List.rev !jobs in
    jobs := [];
    Mutex.unlock event_mtx;
    scheduler.thread_safe_send_emit_events_job (fun () ->
        List.concat_map jobs_batch ~f:(fun job -> job ()));
    Thread.delay debounce_interval;
    buffer_thread ()
  in
  scheduler.spawn_thread buffer_thread;
  res

let create_external ~root ~debounce_interval ~scheduler ~backend =
  match debounce_interval with
  | None -> create_no_buffering ~root ~scheduler ~backend
  | Some debounce_interval ->
    with_buffering ~scheduler ~debounce_interval
      ~create:(create_no_buffering ~root)
      ~backend

let create_inotifylib ~scheduler =
  prepare_sync ();
  let ignored_files = Table.create (module String) 64 in
  let inotify = create_inotifylib_watcher ~ignored_files ~scheduler in
  Inotify_lib.add inotify
    (Path.to_string (Path.build (special_file_for_inotify_sync ())));
  { kind = Fine { inotify }; shutdown = `No_op; ignored_files }

let create_default ~scheduler =
  match select_watcher_backend ~use_inotify_lib:true with
  | (`Inotifywait _ | `Fswatch _) as backend ->
    create_external ~scheduler ~root:Path.root
      ~debounce_interval:(Some 0.5 (* seconds *)) ~backend
  | `Inotify_lib -> create_inotifylib ~scheduler

let create_external ~root ~debounce_interval ~scheduler =
  match select_watcher_backend ~use_inotify_lib:false with
  | (`Inotifywait _ | `Fswatch _) as backend ->
    create_external ~root ~debounce_interval ~scheduler ~backend
  | `Inotify_lib -> assert false

let wait_for_initial_watches_established_blocking t =
  match t.kind with
  | Coarse { wait_for_watches_established } -> wait_for_watches_established ()
  | Fine { inotify = _ } ->
    (* no initial watches needed: all watches should be set up at the time just
       before file access *)
    ()

let add_watch t path =
  match t.kind with
  | Coarse _ ->
    (* Here we assume that the path is already being watched because the coarse
       file watchers are expected to watch all the source files from the
       start *)
    ()
  | Fine { inotify } -> Inotify_lib.add inotify (Path.to_string path)

module For_tests = struct
  let pid t =
    match t.shutdown with
    | `Kill pid -> pid
    | `No_op -> failwith "don't know how to suspend an inotifylib watcher"

  let suspend t = Unix.kill (Pid.to_int (pid t)) Sys.sigstop

  let resume t = Unix.kill (Pid.to_int (pid t)) Sys.sigcont
end

let ignore_next_file_change_event t path =
  assert (Path.is_in_source_tree path);
  Table.set t.ignored_files (Path.to_absolute_filename path) ()
