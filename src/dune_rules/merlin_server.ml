open! Dune_engine
open! Stdune
open Fiber.O

module Merlin_conf = struct
  type t = Sexp.t

  let make_error msg = Sexp.(List [ List [ Atom "ERROR"; Atom msg ] ])

  let to_stdout (t : t) =
    Csexp.to_channel stdout t;
    flush stdout
end

module Commands = struct
  type t =
    | File of string
    | Halt
    | Unknown of string

  let read_input in_channel =
    match Csexp.input in_channel with
    | Ok sexp -> (
      let open Sexp in
      match sexp with
      | Atom "Halt" -> Halt
      | List [ Atom "File"; Atom path ] -> File path
      | sexp ->
        let msg = Printf.sprintf "Bad input: %s" (Sexp.to_string sexp) in
        Unknown msg)
    | Error _ -> Halt
end

(* [make_relative_to_root p] will check that [Path.root] is a prefix of the
   absolute path [p] and remove it if that is the case. Under Windows and Cygwin
   environement both paths are lowarcased before the comparison *)
let make_relative_to_root p =
  let prefix = Path.(to_absolute_filename root) in
  let p = Path.(to_absolute_filename p) in
  let prefix, p =
    if Sys.win32 || Sys.cygwin then
      (String.lowercase_ascii prefix, String.lowercase_ascii p)
    else
      (prefix, p)
  in
  String.drop_prefix ~prefix p
  (* After dropping the prefix we need to remove the leading path separator *)
  |> Option.map ~f:(fun s -> String.drop s 1)

(* [to_local p] makes path [p] relative to the project's root. [p] can be: - An
   absolute path - A path relative to [Path.initial_cwd] *)
let to_local file_path =
  let error msg = Error msg in
  (* This ensure the path is absolute. If not it is prefixed with
     [Path.initial_cwd] *)
  let abs_file_path = Path.of_filename_relative_to_initial_cwd file_path in
  (* Then we make the path relative to [Path.root] (and not
     [Path.initial_cwd]) *)
  match make_relative_to_root abs_file_path with
  | Some path -> (
    try
      let path = Path.of_string path in
      (* If dune ocaml-merlin is called from within the build dir we must remove
         the build context *)
      Ok (Path.drop_optional_build_context path |> Path.local_part)
    with
    | User_error.E (mess, _) -> User_message.to_string mess |> error)
  | None ->
    Printf.sprintf "Path %S is not in dune workspace (%S)." file_path
      Path.(to_absolute_filename Path.root)
    |> error

(* Given a path [p] relative to the workspace root, [get_merlin_files_paths p]
   navigates to the [_build] directory and reaches this path from the correct
   context. Then it returns the list of available Merlin configurations for this
   directory. *)
let get_merlin_files_paths local_path =
  let+ workspace = Memo.Build.run (Workspace.workspace ()) in
  let context =
    Option.value ~default:Context_name.default workspace.merlin_context
  in
  let ctx = Context_name.to_string context in
  let ctx_root = Path.Build.(relative root ctx) in
  let dir_path = Path.Build.(append_local ctx_root local_path) in
  let merlin_path =
    Path.Build.relative dir_path Merlin_ident.merlin_folder_name
  in
  let files =
    Result.value ~default:[] (Path.readdir_unsorted (Path.build merlin_path))
    |> List.fast_sort ~cmp:Stdlib.compare
  in
  List.map files ~f:(fun f -> Path.Build.relative merlin_path f |> Path.build)

let load_merlin_file local_path file =
  (* We search for an appropriate merlin configuration in the current directory
     and its parents *)
  let rec find_closest path =
    let filename = String.lowercase_ascii file in
    let* file_paths = get_merlin_files_paths path in
    let result =
      List.find_map file_paths ~f:(fun file_path ->
          if Path.exists file_path then
            match Merlin.Processed.load_file file_path with
            | Ok config -> Merlin.Processed.get config ~filename
            | Error msg -> Some (Merlin_conf.make_error msg)
          else
            None)
    in
    match result with
    | Some p -> Fiber.return (Some p)
    | None -> (
      match
        if Path.Local.is_root path then
          None
        else
          Path.Local.parent path
      with
      | None -> Fiber.return None
      | Some dir -> find_closest dir)
  in
  let default =
    Printf.sprintf
      "No config found for file %S in %S. Try calling `dune build`." file
      (Path.Local.to_string local_path)
    |> Merlin_conf.make_error
  in
  find_closest local_path >>| function
  | None -> default
  | Some x -> x

let print_merlin_conf file =
  let dir, file = Filename.(dirname file, basename file) in
  let+ answer =
    match to_local dir with
    | Ok p -> load_merlin_file p file
    | Error s -> Fiber.return (Merlin_conf.make_error s)
  in
  Merlin_conf.to_stdout answer

let dump s =
  match to_local s with
  | Ok path ->
    get_merlin_files_paths path >>| List.iter ~f:Merlin.Processed.print_file
  | Error mess ->
    Printf.eprintf "%s\n%!" mess;
    Fiber.return ()

let dump_dot_merlin s =
  match to_local s with
  | Ok path ->
    let+ files = get_merlin_files_paths path in
    Merlin.Processed.print_generic_dot_merlin files
  | Error mess ->
    Printf.eprintf "%s\n%!" mess;
    Fiber.return ()

let start () =
  let rec main () =
    match Commands.read_input stdin with
    | File path ->
      let* () = print_merlin_conf path in
      main ()
    | Unknown msg ->
      Merlin_conf.to_stdout (Merlin_conf.make_error msg);
      main ()
    | Halt -> exit 0
  in
  main ()
