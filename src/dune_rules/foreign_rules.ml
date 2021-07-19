open! Dune_engine
open! Stdune

module Source_tree_map_reduce =
  Source_tree.Dir.Make_map_reduce
    (Action_builder)
    (Monoid.Appendable_list (struct
      type t = Command.Args.without_targets Command.Args.t
    end))

(* Compute command line flags for the [include_dirs] field of [Foreign.Stubs.t]
   and track all files in specified directories as [Hidden_deps]
   dependencies. *)
let include_dir_flags ~expander ~dir (stubs : Foreign.Stubs.t) =
  let scope = Expander.scope expander in
  let lib_dir loc lib_name =
    let open Resolve.O in
    let+ lib = Lib.DB.resolve (Scope.libs scope) (loc, lib_name) in
    Lib_info.src_dir (Lib.info lib)
  in
  Command.Args.S
    (List.map stubs.include_dirs ~f:(fun include_dir ->
         Resolve.args
           (let open Resolve.O in
           let+ loc, include_dir =
             match (include_dir : Foreign.Stubs.Include_dir.t) with
             | Dir dir ->
               Resolve.return
                 (String_with_vars.loc dir, Expander.expand_path expander dir)
             | Lib (loc, lib_name) ->
               let+ lib_dir = lib_dir loc lib_name in
               (loc, Action_builder.return lib_dir)
           in
           Command.Args.Dyn
             (let open Action_builder.O in
             let+ include_dir = include_dir in
             let dep_args =
               match Path.extract_build_context_dir include_dir with
               | None ->
                 (* This branch corresponds to an external directory. The
                    current implementation tracks its contents
                    NON-recursively. *)
                 (* TODO: Track the contents recursively. One way to implement
                    this is to change [Build_system.Loaded.Non_build] so that it
                    contains not only files but also directories and traverse
                    them recursively in [Build_system.Exported.Pred]. *)
                 let () =
                   let error msg =
                     User_error.raise ~loc
                       [ Pp.textf "Unable to read the include directory."
                       ; Pp.textf "Reason: %s." msg
                       ]
                   in
                   match Path.is_directory_with_error include_dir with
                   | Error msg -> error msg
                   | Ok false ->
                     error
                       (Printf.sprintf "%S is not a directory"
                          (Path.to_string include_dir))
                   | Ok true -> ()
                 in
                 let deps =
                   Dep.Set.singleton
                     (Dep.file_selector
                        (File_selector.create ~dir:include_dir Predicate.true_))
                 in
                 Command.Args.Hidden_deps deps
               | Some (build_dir, source_dir) ->
                 let open Action_builder.O in
                 Command.Args.Dyn
                   ((* This branch corresponds to a source directory. We track
                       its contents recursively. *)
                    Action_builder.memo_build (Source_tree.find_dir source_dir)
                    >>= function
                    | None ->
                      User_error.raise ~loc
                        [ Pp.textf "Include directory %S does not exist."
                            (Path.reach ~from:(Path.build dir) include_dir)
                        ]
                    | Some dir ->
                      let+ l =
                        Source_tree_map_reduce.map_reduce dir
                          ~traverse:Sub_dirs.Status.Set.all ~f:(fun t ->
                            let dir =
                              Path.append_source build_dir
                                (Source_tree.Dir.path t)
                            in
                            let deps =
                              Dep.Set.singleton
                                (Dep.file_selector
                                   (File_selector.create ~dir Predicate.true_))
                            in
                            Action_builder.return
                              (Appendable_list.singleton
                                 (Command.Args.Hidden_deps deps)))
                      in
                      Command.Args.S (Appendable_list.to_list l))
             in
             Command.Args.S [ A "-I"; Path include_dir; dep_args ]))))

let build_c ~kind ~sctx ~dir ~expander ~include_flags (loc, src, dst) =
  let ctx = Super_context.context sctx in
  let project = Super_context.find_scope_by_dir sctx dir |> Scope.project in
  let use_standard_flags = Dune_project.use_standard_c_and_cxx_flags project in
  let base_flags =
    let cfg = ctx.ocaml_config in
    match kind with
    | Foreign_language.C -> (
      match use_standard_flags with
      | None
      | Some false ->
        (* In dune < 2.8 flags from ocamlc_config are always added *)
        List.concat
          [ Ocaml_config.ocamlc_cflags cfg
          ; Ocaml_config.ocamlc_cppflags cfg
          ; Fdo.c_flags ctx
          ]
      | Some true -> Fdo.c_flags ctx)
    | Foreign_language.Cxx -> Fdo.cxx_flags ctx
  in
  let open Memo.Build.O in
  let* with_user_and_std_flags =
    let flags = Foreign.Source.flags src in
    (* DUNE3 will have [use_standard_c_and_cxx_flags] enabled by default. To
       guide users toward this change we emit a warning when dune_lang is >=
       1.8, [use_standard_c_and_cxx_flags] is not specified in the
       [dune-project] file (thus defaulting to [true]), the [:standard] set of
       flags has been overridden and we are not in a vendored project *)
    let has_standard = Ordered_set_lang.Unexpanded.has_standard flags in
    let+ is_vendored =
      match Path.Build.drop_build_context dir with
      | Some src_dir -> Dune_engine.Source_tree.is_vendored src_dir
      | None -> Memo.Build.return false
    in
    if
      Dune_project.dune_version project >= (2, 8)
      && Option.is_none use_standard_flags
      && (not is_vendored) && not has_standard
    then
      User_warning.emit ~loc
        [ Pp.text
            "The flag set for these foreign sources overrides the `:standard` \
             set of flags. However the flags in this standard set are still \
             added to the compiler arguments by Dune. This might cause \
             unexpected issues. You can disable this warning by defining the \
             option `(use_standard_c_and_cxx_flags <bool>)` in your \
             `dune-project` file. Setting this option to `true` will \
             effectively prevent Dune from silently adding c-flags to the \
             compiler arguments which is the new recommended behaviour."
        ];
    Super_context.foreign_flags sctx ~dir ~expander ~flags ~language:kind
    |> Action_builder.map ~f:(List.append base_flags)
  and* c_compiler =
    Super_context.resolve_program ~loc:None ~dir sctx
      (Ocaml_config.c_compiler ctx.ocaml_config)
  in
  let output_param =
    match ctx.lib_config.ccomp_type with
    | Msvc -> [ Command.Args.Concat ("", [ A "/Fo"; Target dst ]) ]
    | Other _ -> [ A "-o"; Target dst ]
  in
  let+ () =
    Super_context.add_rule sctx ~loc
      ~dir
        (* With sandboxing we get errors like: bar.c:2:19: fatal error: foo.cxx:
           No such file or directory #include "foo.cxx". (These errors happen
           only when compiling c files.) *)
      ~sandbox:Sandbox_config.no_sandboxing
      (let src = Path.build (Foreign.Source.path src) in
       (* We have to execute the rule in the library directory as the .o is
          produced in the current directory *)
       Command.run ~dir:(Path.build dir) c_compiler
         ([ Command.Args.dyn with_user_and_std_flags
          ; S [ A "-I"; Path ctx.stdlib_dir ]
          ; include_flags
          ]
         @ output_param @ [ A "-c"; Dep src ]))
  in
  dst

(* TODO: [requires] is a confusing name, probably because it's too general: it
   looks like it's a list of libraries we depend on. *)
let build_o_files ~sctx ~foreign_sources ~(dir : Path.Build.t) ~expander
    ~requires ~dir_contents =
  let ctx = Super_context.context sctx in
  let all_dirs = Dir_contents.dirs dir_contents in
  let h_files =
    List.fold_left all_dirs ~init:[] ~f:(fun acc dc ->
        String.Set.fold (Dir_contents.text_files dc) ~init:acc ~f:(fun fn acc ->
            if String.is_suffix fn ~suffix:Foreign_language.header_extension
            then
              Path.relative (Path.build (Dir_contents.dir dc)) fn :: acc
            else
              acc))
  in
  let includes =
    Command.Args.S
      [ Hidden_deps (Dep.Set.of_files h_files)
      ; Resolve.args
          (let open Resolve.O in
          let+ libs = requires in
          Command.Args.S
            [ Lib.L.c_include_flags libs
            ; Hidden_deps
                (Lib_file_deps.deps libs ~groups:[ Lib_file_deps.Group.Header ])
            ])
      ]
  in
  String.Map.to_list_map foreign_sources ~f:(fun obj (loc, src) ->
      let dst = Path.Build.relative dir (obj ^ ctx.lib_config.ext_obj) in
      let stubs = src.Foreign.Source.stubs in
      let extra_flags = include_dir_flags ~expander ~dir src.stubs in
      let extra_deps =
        let open Action_builder.O in
        let+ () = Dep_conf_eval.unnamed stubs.extra_deps ~expander in
        Command.Args.empty
      in
      let include_flags =
        Command.Args.S [ includes; extra_flags; Dyn extra_deps ]
      in
      let build_file =
        match Foreign.Source.language src with
        | C -> build_c ~kind:Foreign_language.C
        | Cxx -> build_c ~kind:Foreign_language.Cxx
      in
      build_file ~sctx ~dir ~expander ~include_flags (loc, src, dst))
