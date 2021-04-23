(* [execve] doesn't exist on Windows, so instead we do a
   [Unix.create_process_env] followed by [Unix.waitpid] and finally [sys_exit].
   We use [sys_exit] rather than [exit] so that [at_exit] functions are not
   invoked. We don't want [at_exit] functions to be invoked to match the
   behaviour of [Unix.execve] on Unix. *)
external sys_exit : int -> 'a = "caml_sys_exit"

let restore_cwd_and_execve prog argv ~env =
  let env = Env.to_unix env |> Array.of_list in
  let argv = Array.of_list argv in
  Sys.chdir (Path.External.to_string Path.External.initial_cwd);
  if Sys.win32 then
    let pid =
      Unix.create_process_env prog argv env Unix.stdin Unix.stdout Unix.stderr
    in
    match snd (Unix.waitpid [] pid) with
    | WEXITED n -> sys_exit n
    | WSIGNALED _ -> sys_exit 255
    | WSTOPPED _ -> assert false
  else (
    ignore (Unix.sigprocmask SIG_SETMASK [] : int list);
    Stdlib.do_at_exit ();
    Unix.execve prog argv env
  )

type resource_usage =
  { utime : float
  ; stime : float
  }

external stub_wait3 :
  Unix.wait_flag list -> int * Unix.process_status * resource_usage
  = "dune_wait3"

let wait3 flags =
  if Sys.win32 then
    Code_error.raise "wait3 not available on windows" []
  else
    stub_wait3 flags
