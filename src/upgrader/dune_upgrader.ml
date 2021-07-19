open! Dune_engine
open! Stdune

type rename_and_edit =
  { original_file : Path.Source.t
  ; extra_files_to_delete : Path.Source.t list
  ; new_file : Path.Source.t
  ; contents : string
  }

type todo =
  { mutable to_rename_and_edit : rename_and_edit list
  ; mutable to_edit : (Path.Source.t * string) list
  }

type project_version =
  | Unknown
  | Dune2_project
  | Dune1_project

module Common = struct
  module Ast_tools = struct
    open Dune_lang.Ast

    let field_of_list ?more:(m = []) atoms =
      List (Loc.none, List.map atoms ~f:(fun a -> Atom (Loc.none, a)) @ m)

    let make_foreign_stubs lang names flags =
      let open Dune_lang.Atom in
      let add_more name olist m =
        match olist with
        | Some (_ :: more) -> field_of_list [ of_string name ] ~more :: m
        | _ -> m
      in

      let more =
        List.rev
          ([ field_of_list [ of_string "language"; of_string lang ] ]
          |> add_more "names" names |> add_more "flags" flags)
      in

      field_of_list [ of_string "foreign_stubs" ] ~more

    let rec replace_first old_name new_name = function
      | List (loc, Atom (loca, A atom) :: tll) :: tl when atom = old_name ->
        List (loc, Atom (loca, Dune_lang.Atom.of_string new_name) :: tll) :: tl
      | List (loc, Quoted_string (loca, str) :: tll) :: tl when str = old_name
        ->
        List (loc, Quoted_string (loca, new_name) :: tll) :: tl
      | hd :: tl -> hd :: replace_first old_name new_name tl
      | [] -> []

    let extract_first names =
      let rec is_names vals names =
        match (vals, names) with
        | (Atom (_, A str) | Quoted_string (_, str)) :: tl, name :: tln
          when str = name ->
          is_names tl tln
        | _, [] -> true
        | _, _ -> false
      in
      let rec aux rest = function
        | List (_, elt) :: tl when is_names elt names ->
          (Some elt, List.rev_append rest tl)
        | hd :: tl -> aux (hd :: rest) tl
        | [] -> (None, List.rev rest)
      in
      aux []

    let is_in_fields names fields =
      match fst (extract_first names fields) with
      | Some _ -> true
      | None -> false

    let bump_lang_version v =
      let v = Dune_lang.Syntax.Version.to_string v in
      function
      | List
          ( loc
          , (Atom (_, A "lang") as lang)
            :: (Atom (_, A "dune") as dune) :: Atom (loc3, A _) :: tll )
        :: tl ->
        List
          (loc, lang :: dune :: Atom (loc3, Dune_lang.Atom.of_string v) :: tll)
        :: tl
      | sexp -> sexp

    let included_file path = function
      | List
          ( _
          , [ Atom (_, A "include")
            ; (Atom (loc, A fn) | Quoted_string (loc, fn))
            ] ) ->
        let dir = Path.Source.parent_exn path in
        let included_file = Path.Source.relative dir fn in
        if not (Path.exists (Path.source included_file)) then
          User_error.raise ~loc
            [ Pp.textf "File %s doesn't exist."
                (Path.Source.to_string_maybe_quoted included_file)
            ];
        Some included_file
      | _ -> None

    let included_files_paths path = List.filter_map ~f:(included_file path)
  end

  let read_and_parse path =
    let csts = Dune_lang.Parser.load (Path.source path) ~mode:Cst in
    let comments = Dune_lang.Cst.extract_comments csts in
    let sexps = List.filter_map csts ~f:Dune_lang.Cst.abstract in
    (sexps, comments)

  (* Return a mapping [Path.t -> Dune_lang.Ast.t list] containing [path] and all
     the files in includes, recursiverly *)
  let scan_included_files path =
    let files = ref Path.Source.Map.empty in
    let rec iter path =
      if not (Path.Source.Map.mem !files path) then (
        let sexps, comments = read_and_parse path in
        files := Path.Source.Map.set !files path (sexps, comments);
        List.iter (Ast_tools.included_files_paths path sexps) ~f:iter
      )
    in
    iter path;
    !files

  let string_of_sexps ~version sexps comments =
    let new_csts = List.map sexps ~f:Dune_lang.Cst.concrete in
    Dune_lang.Parser.insert_comments new_csts comments
    |> Format_dune_lang.pp_top_sexps ~version
    |> Format.asprintf "%a@?" Pp.to_fmt

  let ensure_project_file_exists project ~lang_version =
    let fn = Path.source (Dune_project.file project) in
    if not (Path.exists fn) then (
      Console.print
        [ Pp.textf "Creating %s..." (Path.to_string_maybe_quoted fn) ];
      Io.write_lines fn ~binary:false
        (List.concat
           [ [ sprintf "(lang dune %s)"
                 (Dune_lang.Syntax.Version.to_string lang_version)
             ]
           ; (match Dune_project.name project with
             | Anonymous _ -> []
             | Named s ->
               [ Dune_lang.to_string
                   (List
                      [ Dune_lang.atom "name"
                      ; Dune_lang.atom_or_quoted_string s
                      ])
               ])
           ])
    )
end

module V2 = struct
  open Common

  (* If no mode is defined, explicitly use the previous default *)
  let explicit_mode fields =
    if Ast_tools.is_in_fields [ "modes" ] fields then
      fields
    else
      Dune_lang.Atom.(
        Ast_tools.field_of_list
          [ of_string "modes"; of_string "byte"; of_string "exe" ])
      :: fields

  (* Field preprocessor_deps cannot exist without field preprocess *)
  let no_single_preprocessor_deps fields =
    match Ast_tools.extract_first [ "preprocessor_deps" ] fields with
    | Some _, rest
      when Ast_tools.is_in_fields [ "preprocess"; "no_preprocessing" ] rest ->
      rest
    | Some _, rest when Ast_tools.is_in_fields [ "preprocess" ] rest -> fields
    | Some _, rest -> rest
    | None, _ -> fields

  (* Always no-op no_keep_loc field should be removed *)
  let no_no_keep_loc fields =
    match Ast_tools.extract_first [ "no_keep_locs" ] fields with
    | Some _, rest
    | None, rest ->
      rest

  (* c_names, c_flags, cxx_names and cxx_flags -> foreign_stubs *)
  let to_foreign_stubs fields =
    let aux lang fields =
      let names, rest = Ast_tools.extract_first [ lang ^ "_names" ] fields in
      let flags, rest = Ast_tools.extract_first [ lang ^ "_flags" ] rest in
      match (names, flags) with
      | None, None -> fields
      | _ -> Ast_tools.make_foreign_stubs lang names flags :: rest
    in
    fields |> aux "c" |> aux "cxx"

  let update_stanza =
    let open Dune_lang.Ast in
    function
    | List (loc, Atom (loca, A "alias") :: tl) as ast ->
      if Ast_tools.is_in_fields [ "action" ] tl then
        let tl = Ast_tools.replace_first "name" "alias" tl in
        List (loc, Atom (loca, Dune_lang.Atom.of_string "rule") :: tl)
      else
        ast
    | List (loc, Atom (loca, (A "executable" as atom)) :: tl)
    | List (loc, Atom (loca, (A "executables" as atom)) :: tl) ->
      let tl =
        tl |> no_single_preprocessor_deps |> explicit_mode |> to_foreign_stubs
      in
      List (loc, Atom (loca, atom) :: tl)
    | List (loc, Atom (loca, (A "library" as atom)) :: tl) ->
      let tl =
        tl |> no_single_preprocessor_deps |> no_no_keep_loc |> to_foreign_stubs
      in
      List (loc, Atom (loca, atom) :: tl)
    | stanza -> stanza

  let update_formatting sexps =
    let elt, sexps = Ast_tools.extract_first [ "using"; "fmt" ] sexps in
    match elt with
    (* Was not using fmt *)
    | None ->
      sexps
      @ [ Ast_tools.field_of_list
            Dune_lang.Atom.[ of_string "formatting"; of_string "disabled" ]
        ]
    (* Was using fmt *)
    | Some [ _; _; _ ] -> sexps
    (* Was using fmt enabled_for *)
    | Some (_ :: _ :: _ :: tl) ->
      sexps
      @ [ Ast_tools.field_of_list
            Dune_lang.Atom.[ of_string "formatting" ]
            ~more:tl
        ]
    (* Unexpected *)
    | _ -> sexps

  let update_project_file todo project =
    let sexps, comments = read_and_parse (Dune_project.file project) in
    let v = !Dune_project.default_dune_language_version in
    let sexps = sexps |> Ast_tools.bump_lang_version v |> update_formatting in
    let new_file_contents = string_of_sexps ~version:v sexps comments in
    (* Printf.eprintf "AAAH: %b\n!" (was_formatted sexps); *)
    todo.to_edit <-
      (Dune_project.file project, new_file_contents) :: todo.to_edit

  let upgrade_dune_file todo fn sexps comments =
    let new_ast = sexps |> List.map ~f:update_stanza in
    let version = !Dune_project.default_dune_language_version in
    let new_file_contents = string_of_sexps ~version new_ast comments in
    todo.to_edit <- (fn, new_file_contents) :: todo.to_edit

  let upgrade_dune_files todo dir =
    if String.Set.mem (Source_tree.Dir.files dir) Source_tree.Dune_file.fname
    then
      let path = Source_tree.Dir.path dir in
      let fn = Path.Source.relative path Source_tree.Dune_file.fname in
      if Io.with_lexbuf_from_file (Path.source fn) ~f:Dune_lexer.is_script then
        User_warning.emit
          ~loc:(Loc.in_file (Path.source fn))
          [ Pp.text "Cannot upgrade this file as it is using the OCaml syntax."
          ; Pp.text "You need to upgrade it manually."
          ]
      else
        let files = scan_included_files fn in
        Path.Source.Map.iteri files ~f:(fun fn' (sexps, comments) ->
            upgrade_dune_file todo fn' sexps comments)

  let todo_log =
    {|
- If you use generated dune.inc files you probably should update your generators.
- mli only modules must now be explicitly declared. This was previously a
  warning and is now an error.
- Stop installing the `ocaml-syntax-shims` binary. In order to use
  `future_syntax`, one now need to depend on the `ocaml-syntax-shims`
  package.
- Actions which introduce targets where new targets are forbidden (e.g.
  preprocessing) are now an error instead of a warning.
- Stop installing the `ocaml-syntax-shims` binary. In order to use
  `future_syntax`, one now need to depend on the `ocaml-syntax-shims`
  package.
- Do not put the `<package>.install` files in the source tree unless `-p` or
  `--promote-install-files` is passed on the command line
- Library names are now validated in a strict fashion. Previously, invalid names
  would be allowed for unwrapped libraries
- Stricter validation of file names in `select`. The file names of conditional
  sources must match the prefix and the extension of the resultant filename.
- Modules filtered out from the module list via the Ordered Set Language must
  now be actual modules.
- Stub names are no longer allowed relative paths. This was previously a warning
  and is now an error.
- In `(diff? x y)` action, require `x` to exist and register a
  dependency on that file.
- `self_build_stubs_archive` was deleted in version 2.0 of the dune
language. Use the (foreign_archives ...) field instead.|}

  let upgrade todo dir =
    let lang_version = (2, 0) in
    Dune_project.default_dune_language_version := lang_version;
    let project = Source_tree.Dir.project dir in
    if Dune_project.root project = Source_tree.Dir.path dir then
      ensure_project_file_exists project ~lang_version;
    update_project_file todo project;
    upgrade_dune_files todo dir
end

let detect_project_version project dir =
  let in_tree = String.Set.mem (Source_tree.Dir.files dir) in
  Dune_project.default_dune_language_version := (0, 1);
  if in_tree "jbuild" then (
    let fn = Path.relative (Path.source (Source_tree.Dir.path dir)) "jbuild" in
    User_warning.emit ~loc:(Loc.in_file fn)
      [ Pp.text
          "Since Dune 3.0.0 it is no longer possible to upgrade jbuilder \
           projects. You need to use an older version of Dune to upgrade this \
           project."
      ];
    Unknown
  ) else
    let project_dune_version = Dune_project.dune_version project in
    let open Dune_lang.Syntax.Version.Infix in
    if project_dune_version >= (2, 0) then
      Dune2_project
    else if project_dune_version >= (1, 0) then
      Dune1_project
    else if in_tree Source_tree.Dune_file.fname then
      Dune1_project
    else
      Unknown

let upgrade () =
  let open Fiber.O in
  let rec aux last =
    let todo = { to_rename_and_edit = []; to_edit = [] } in
    let* current_versions =
      Memo.Build.run
        (let module M =
           Source_tree.Make_map_reduce_with_progress
             (Memo.Build)
             (Monoid.Appendable_list (struct
               type t = Source_tree.Dir.t * project_version
             end))
         in
        M.map_reduce ~traverse:Sub_dirs.Status.Set.normal_only ~f:(fun dir ->
            let project = Source_tree.Dir.project dir in
            let detected_version = detect_project_version project dir in
            Memo.Build.return
              (Appendable_list.singleton (dir, detected_version))))
      >>| Appendable_list.to_list
    in
    let v1_updates = ref false in
    let v2_updates = ref false in
    let log_update dir ver =
      Console.print
        [ Pp.textf "Project in dir %s will be upgraded to dune %s." dir ver ]
    in
    List.iter current_versions ~f:(fun (dir, version) ->
        let d = Path.Source.to_string_maybe_quoted (Source_tree.Dir.path dir) in
        match version with
        | Unknown -> ()
        | Dune1_project ->
          log_update d "v2";
          v2_updates := true;
          V2.upgrade todo dir
        | Dune2_project -> ());
    List.iter todo.to_edit ~f:(fun (fn, s) ->
        Console.print
          [ Pp.textf "Upgrading %s..." (Path.Source.to_string_maybe_quoted fn) ];
        Io.write_file (Path.source fn) s ~binary:true);
    List.iter todo.to_rename_and_edit ~f:(fun x ->
        let { original_file; new_file; extra_files_to_delete; contents } = x in
        Console.print
          [ Pp.textf "Upgrading %s to %s..."
              (List.map
                 (extra_files_to_delete @ [ original_file ])
                 ~f:Path.Source.to_string_maybe_quoted
              |> String.enumerate_and)
              (Path.Source.to_string_maybe_quoted new_file)
          ];
        List.iter (original_file :: extra_files_to_delete) ~f:(fun p ->
            Path.unlink (Path.source p));
        Io.write_file (Path.source new_file) contents ~binary:true);
    if !v1_updates && not last then (
      (* Run the upgrader again to update new v1 projects to v2 No more than one
         additional upgrade should be needed *)
      (* We reset memoization tables as a simple way to refresh the
         Source_tree *)
      Memo.reset Memo.Invalidation.clear_caches;
      aux true
    ) else if !v2_updates then (
      Console.print
        [ Pp.textf
            "\n\
             Some projects were upgraded to dune v2. Some breaking changes may \
             not\n\
             have been treated automatically. Here is a list of things you \
             should check\n\
             to complete the migration:\n\
             %s"
            V2.todo_log
        ];
      Fiber.return ()
    ) else
      Fiber.return ()
  in
  aux false
