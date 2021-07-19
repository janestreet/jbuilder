open Stdune
open Dune_engine
open Fiber.O
open! Dune_tests_common
module Config = Dune_util.Config

let () = init ()

let printf = Printf.printf

let temp_dir = lazy (Path.of_string "vcs-tests")

let () = at_exit (fun () -> Path.rm_rf (Lazy.force temp_dir))

(* When hg is not available, we test with git twice indeed. This is because many
   people don't have hg installed. *)
let has_hg =
  match Lazy.force Vcs.hg with
  | (_ : Path.t) -> true
  | exception _ -> false

let run (vcs : Vcs.t) args =
  let prog, prog_str, real_args =
    match vcs.kind with
    | Git -> (Vcs.git, "git", args)
    | Hg -> (
      if has_hg then
        (Vcs.hg, "hg", args)
      else
        ( Vcs.git
        , "hg"
        , match args with
          | [ "tag"; s; "-u"; _ ] -> [ "tag"; "-a"; s; "-m"; s ]
          | [ "commit"; "-m"; msg; "-u"; _ ] -> [ "commit"; "-m"; msg ]
          | _ -> args ))
  in
  printf "$ %s\n"
    (List.map (prog_str :: args) ~f:String.quote_for_shell
    |> String.concat ~sep:" ");
  Process.run Strict (Lazy.force prog) real_args
    ~env:
      ((* One of the reasons to set GIT_DIR is to override any GIT_DIR set by
          the environment, which helps for example during [git rebase
          --exec]. *)
       Env.add Env.initial ~var:"GIT_DIR"
         ~value:(Filename.concat (Path.to_absolute_filename vcs.root) ".git"))
    ~dir:vcs.root
    ~stdout_to:(Process.Io.file Config.dev_null Process.Io.Out)

type action =
  | Init
  | Add of string
  | Write of string * string
  | Commit
  | Tag of string
  | Describe of string

let run_action (vcs : Vcs.t) action =
  match action with
  | Init -> run vcs [ "init"; "-q" ]
  | Add fn -> run vcs [ "add"; fn ]
  | Commit -> (
    match vcs.kind with
    | Git -> run vcs [ "commit"; "-m"; "commit message" ]
    | Hg -> run vcs [ "commit"; "-m"; "commit message"; "-u"; "toto" ])
  | Write (fn, s) ->
    printf "$ echo %S > %s\n" s fn;
    Io.write_file (Path.relative (Lazy.force temp_dir) fn) s;
    Fiber.return ()
  | Describe expected ->
    printf "$ %s describe [...]\n"
      (match vcs.kind with
      | Git -> "git"
      | Hg -> "hg");
    Memo.reset Memo.Invalidation.clear_caches;
    let vcs =
      match vcs.kind with
      | Hg when not has_hg -> { vcs with kind = Git }
      | _ -> vcs
    in
    let+ s = Memo.Build.run (Vcs.describe vcs) in
    let s = Option.value s ~default:"n/a" in
    let processed =
      String.split s ~on:'-'
      |> List.map ~f:(fun s ->
             match s with
             | ""
             | "dirty" ->
               s
             | s
               when String.length s = 1
                    && String.for_all s ~f:(function
                         | '0' .. '9' -> true
                         | _ -> false) ->
               s
             | _
               when String.for_all s ~f:(function
                      | '0' .. '9'
                      | 'a' .. 'z' ->
                        true
                      | _ -> false) ->
               "<commit-id>"
             | _ -> s)
      |> String.concat ~sep:"-"
    in
    printf "%s\n" processed;
    if processed <> expected then
      printf "Expected: %s\nOriginal: %s\n" expected s;
    printf "\n"
  | Tag s -> (
    match vcs.kind with
    | Git -> run vcs [ "tag"; "-a"; s; "-m"; s ]
    | Hg -> run vcs [ "tag"; s; "-u"; "toto" ])

let run kind script =
  let (lazy temp_dir) = temp_dir in
  Path.rm_rf temp_dir;
  Path.mkdir_p temp_dir;
  let vcs = { Vcs.kind; root = temp_dir } in
  let config =
    { Scheduler.Config.concurrency = 1
    ; display = { verbosity = Short; status_line = false }
    ; rpc = None
    ; stats = None
    }
  in
  Scheduler.Run.go
    ~on_event:(fun _ _ -> ())
    config
    (fun () -> Fiber.sequential_iter script ~f:(run_action vcs))

let script =
  [ Init
  ; Write ("a", "-")
  ; Add "a"
  ; Commit
  ; Describe "<commit-id>"
  ; Write ("b", "-")
  ; Add "b"
  ; Describe "<commit-id>-dirty"
  ; Commit
  ; Describe "<commit-id>"
  ; Tag "1.0"
  ; Describe "1.0"
  ; Write ("c", "-")
  ; Add "c"
  ; Describe "1.0-dirty"
  ; Commit
  ; Describe "1.0-1-<commit-id>"
  ; Write ("d", "-")
  ; Add "d"
  ; Describe "1.0-1-<commit-id>-dirty"
  ; Commit
  ; Describe "1.0-2-<commit-id>"
  ]

let%expect_test _ =
  run Git script;
  [%expect
    {|
$ git init -q
$ echo "-" > a
$ git add a
$ git commit -m 'commit message'
$ git describe [...]
<commit-id>

$ echo "-" > b
$ git add b
$ git describe [...]
<commit-id>-dirty

$ git commit -m 'commit message'
$ git describe [...]
<commit-id>

$ git tag -a 1.0 -m 1.0
$ git describe [...]
1.0

$ echo "-" > c
$ git add c
$ git describe [...]
1.0-dirty

$ git commit -m 'commit message'
$ git describe [...]
1.0-1-<commit-id>

$ echo "-" > d
$ git add d
$ git describe [...]
1.0-1-<commit-id>-dirty

$ git commit -m 'commit message'
$ git describe [...]
1.0-2-<commit-id>
|}]

let%expect_test _ =
  run Hg script;
  [%expect
    {|
$ hg init -q
$ echo "-" > a
$ hg add a
$ hg commit -m 'commit message' -u toto
$ hg describe [...]
<commit-id>

$ echo "-" > b
$ hg add b
$ hg describe [...]
<commit-id>-dirty

$ hg commit -m 'commit message' -u toto
$ hg describe [...]
<commit-id>

$ hg tag 1.0 -u toto
$ hg describe [...]
1.0

$ echo "-" > c
$ hg add c
$ hg describe [...]
1.0-dirty

$ hg commit -m 'commit message' -u toto
$ hg describe [...]
1.0-1-<commit-id>

$ echo "-" > d
$ hg add d
$ hg describe [...]
1.0-1-<commit-id>-dirty

$ hg commit -m 'commit message' -u toto
$ hg describe [...]
1.0-2-<commit-id>
|}]
