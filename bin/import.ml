open Stdune
open Dune_engine
module Term = Cmdliner.Term
module Manpage = Cmdliner.Manpage
module Super_context = Dune_rules.Super_context
module Context = Dune_rules.Context
module Config = Dune_util.Config
module Local_install_path = Dune_engine.Local_install_path
module Lib_name = Dune_engine.Lib_name
module Build_system = Dune_engine.Build_system
module Findlib = Dune_rules.Findlib
module Package = Dune_engine.Package
module Dune_package = Dune_rules.Dune_package
module Hooks = Dune_engine.Hooks
module Action_builder = Dune_engine.Action_builder
module Action = Dune_engine.Action
module Dep = Dune_engine.Dep
module Action_to_sh = Dune_engine.Action_to_sh
module Dpath = Dune_engine.Dpath
module Install = Dune_engine.Install
module Section = Dune_engine.Section
module Watermarks = Dune_rules.Watermarks
module Promotion = Dune_engine.Promotion
module Colors = Dune_rules.Colors
module Dune_project = Dune_engine.Dune_project
module Workspace = Dune_rules.Workspace
module Cached_digest = Dune_engine.Cached_digest
module Profile = Dune_rules.Profile
module Log = Dune_util.Log
module Dune_rpc = Dune_rpc_private
include Common.Let_syntax

let in_group (t, info) = (Term.Group.Term t, info)

module Main : sig
  include module type of struct
    include Dune_rules.Main
  end

  val setup : unit -> build_system Fiber.t
end = struct
  include Dune_rules.Main

  let setup () =
    let open Fiber.O in
    let* setup = Memo.Build.run (get ()) in
    let* scheduler = Scheduler.t () in
    Console.Status_line.set_live (fun () ->
        let progression = Build_system.get_current_progress () in
        Some
          (Pp.verbatim
             (sprintf "Done: %u/%u (jobs: %u)"
                progression.number_of_rules_executed
                progression.number_of_rules_discovered
                (Scheduler.running_jobs_count scheduler))));
    Fiber.return setup
end

module Scheduler = struct
  include Dune_engine.Scheduler

  let maybe_clear_screen (dune_config : Dune_config.t) =
    match dune_config.terminal_persistence with
    | Clear_on_rebuild -> Console.reset ()
    | Preserve ->
      Console.print_user_message
        (User_message.make
           [ Pp.nop
           ; Pp.tag User_message.Style.Success
               (Pp.verbatim "********** NEW BUILD **********")
           ; Pp.nop
           ])

  let on_event dune_config _config = function
    | Scheduler.Run.Event.Tick -> Console.Status_line.refresh ()
    | Scheduler.Run.Event.Source_files_changed -> maybe_clear_screen dune_config
    | Build_interrupted ->
      let status_line =
        Some
          (Pp.seq
             (* XXX Why do we print "Had errors"? The user simply edited a
                file *)
             (Pp.tag User_message.Style.Error (Pp.verbatim "Had errors"))
             (Pp.verbatim ", killing current build..."))
      in
      Console.Status_line.set_constant status_line
    | Build_finish build_result ->
      let message =
        match build_result with
        | Success -> Pp.tag User_message.Style.Success (Pp.verbatim "Success")
        | Failure -> Pp.tag User_message.Style.Error (Pp.verbatim "Had errors")
      in
      Console.Status_line.set_constant
        (Some
           (Pp.seq message (Pp.verbatim ", waiting for filesystem changes...")))

  let go ~(common : Common.t) ~config:dune_config f =
    let stats = Common.stats common in
    let config = Dune_config.for_scheduler dune_config None stats in
    Scheduler.Run.go config ~on_event:(on_event dune_config) f

  let go_with_rpc_server_and_console_status_reporting ~(common : Common.t)
      ~config:dune_config run =
    let stats = Common.stats common in
    let rpc_where = Some (Dune_rpc_impl.Where.default ()) in
    let config = Dune_config.for_scheduler dune_config rpc_where stats in
    let file_watcher = Common.file_watcher common in
    let run =
      match Common.rpc common with
      | None -> run
      | Some rpc ->
        fun () ->
          Fiber.fork_and_join_unit
            (fun () ->
              let rpc_config = Dune_rpc_impl.Server.config rpc in
              Dune_rpc_impl.Run.run rpc_config config.stats)
            run
    in
    Scheduler.Run.go config ~file_watcher ~on_event:(on_event dune_config) run
end

let restore_cwd_and_execve (common : Common.t) prog argv env =
  let prog =
    if Filename.is_relative prog then
      let root = Common.root common in
      Filename.concat root.dir prog
    else
      prog
  in
  Proc.restore_cwd_and_execve prog argv ~env

(* Adapted from
   https://github.com/ocaml/opam/blob/fbbe93c3f67034da62d28c8666ec6b05e0a9b17c/src/client/opamArg.ml#L759 *)
let command_alias cmd name =
  let term, info = cmd in
  let orig = Term.name info in
  let doc = Printf.sprintf "An alias for $(b,%s)." orig in
  let man =
    [ `S "DESCRIPTION"
    ; `P
        (Printf.sprintf "$(mname)$(b, %s) is an alias for $(mname)$(b, %s)."
           name orig)
    ; `P (Printf.sprintf "See $(mname)$(b, %s --help) for details." orig)
    ; `Blocks Common.help_secs
    ]
  in
  (term, Term.info name ~docs:"COMMAND ALIASES" ~doc ~man)
