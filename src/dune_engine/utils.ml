open! Stdune
open Import

let system_shell_exn =
  let cmd, arg, os =
    if Sys.win32 then
      ("cmd", "/c", "on Windows")
    else
      ("sh", "-c", "")
  in
  let bin = lazy (Bin.which ~path:(Env.path Env.initial) cmd) in
  fun ~needed_to ->
    match Lazy.force bin with
    | Some path -> (path, arg)
    | None ->
      User_error.raise
        [ Pp.textf
            "I need %s to %s but I couldn't find it :(\nWho doesn't have %s%s?!"
            cmd needed_to cmd os
        ]

let bash_exn =
  let bin = lazy (Bin.which ~path:(Env.path Env.initial) "bash") in
  fun ~needed_to ->
    match Lazy.force bin with
    | Some path -> path
    | None ->
      User_error.raise
        [ Pp.textf "I need bash to %s but I couldn't find it :(" needed_to ]

let not_found fmt ?loc ?context ?hint x =
  User_error.make ?loc
    (Pp.textf fmt (String.maybe_quoted x)
    ::
    (match context with
    | None -> []
    | Some name -> [ Pp.textf " (context: %s)" (Context_name.to_string name) ])
    )
    ~hints:
      (match hint with
      | None -> []
      | Some hint -> [ Pp.text hint ])

let program_not_found_message ?context ?hint ~loc prog =
  not_found "Program %s not found in the tree or in PATH" ?context ?hint ?loc
    prog

let program_not_found ?context ?hint ~loc prog =
  raise (User_error.E (program_not_found_message ?context ?hint ~loc prog, []))

let library_not_found ?context ?hint lib =
  raise (User_error.E (not_found "Library %s not found" ?context ?hint lib, []))

let install_file ~(package : Package.Name.t) ~findlib_toolchain =
  let package = Package.Name.to_string package in
  match findlib_toolchain with
  | None -> package ^ ".install"
  | Some x -> sprintf "%s-%s.install" package (Context_name.to_string x)

let line_directive ~filename:fn ~line_number =
  let directive =
    if Foreign_language.has_foreign_extension ~fn then
      "line"
    else
      ""
  in
  sprintf "#%s %d %S\n" directive line_number fn

let local_bin p = Path.Build.relative p ".bin"

let pp_command_hint command =
  let open Pp.O in
  Pp.textf "try:" ++ Pp.newline ++ Pp.cut
  ++ Pp.hbox (Pp.textf "  " ++ Pp.verbatim command)
