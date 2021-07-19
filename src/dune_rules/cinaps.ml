open! Dune_engine
open Import
open! No_io

type t =
  { loc : Loc.t
  ; files : Predicate_lang.Glob.t
  ; libraries : Lib_dep.t list
  ; preprocess : Preprocess.Without_instrumentation.t Preprocess.Per_module.t
  ; preprocessor_deps : Dep_conf.t list
  ; flags : Ocaml_flags.Spec.t
  }

let name = "cinaps"

type Stanza.t += T of t

let syntax =
  Dune_lang.Syntax.create ~name ~desc:"the cinaps extension"
    [ ((1, 0), `Since (1, 11)) ]

let alias = Alias.make (Alias.Name.of_string name)

let decode =
  let open Dune_lang.Decoder in
  fields
    (let+ loc = loc
     and+ files =
       field "files" Predicate_lang.Glob.decode ~default:Predicate_lang.any
     and+ preprocess, preprocessor_deps = Dune_file.preprocess_fields
     and+ libraries =
       field "libraries" (Dune_file.Lib_deps.decode Executable) ~default:[]
     and+ flags = Ocaml_flags.Spec.decode in
     { loc; files; libraries; preprocess; preprocessor_deps; flags })

let () =
  let open Dune_lang.Decoder in
  Dune_project.Extension.register_simple syntax
    (return [ (name, decode >>| fun x -> [ T x ]) ])

let gen_rules sctx t ~dir ~scope =
  let open Memo.Build.O in
  let loc = t.loc in
  (* Files checked by cinaps *)
  let* cinapsed_files =
    Source_tree.files_of (Path.Build.drop_build_context_exn dir)
    >>| Path.Source.Set.to_list
    >>| List.filter_map ~f:(fun p ->
            if
              Predicate_lang.Glob.exec t.files (Path.Source.basename p)
                ~standard:Predicate_lang.any
            then
              Some
                (Path.Build.append_source (Super_context.context sctx).build_dir
                   p)
            else
              None)
  and* prog =
    Super_context.resolve_program sctx ~dir ~loc:(Some loc) name
      ~hint:"opam install cinaps"
  in
  let cinaps_dir =
    let stamp =
      let digest =
        if cinapsed_files = [] then
          Digest.generic (t.loc, t.libraries, t.preprocess, t.preprocessor_deps)
        else
          Digest.generic
            (cinapsed_files, t.libraries, t.preprocess, t.preprocessor_deps)
      in
      String.take (Digest.to_string digest) 8
    in
    Path.Build.relative dir ("." ^ name ^ "." ^ stamp)
  in
  let main_module_name = Module_name.of_string name in
  let module_ =
    Module.generated main_module_name ~src_dir:(Path.build cinaps_dir)
  in
  let cinaps_ml =
    Module.source ~ml_kind:Ml_kind.Impl module_
    |> Option.value_exn |> Module.File.path |> Path.as_in_build_dir_exn
  in
  let cinaps_exe = Path.Build.relative cinaps_dir (name ^ ".exe") in
  let* () =
    (* Ask cinaps to produce a .ml file to build *)
    Super_context.add_rule sctx ~loc:t.loc ~dir
      (Command.run ~dir:(Path.build dir) prog
         [ A "-staged"
         ; Target cinaps_ml
         ; Deps (List.map cinapsed_files ~f:Path.build)
         ])
  and* expander = Super_context.expander sctx ~dir in
  let* preprocess =
    Preprocessing.make sctx ~dir ~expander
      ~lint:(Preprocess.Per_module.no_preprocessing ())
      ~preprocess:t.preprocess ~preprocessor_deps:t.preprocessor_deps
      ~instrumentation_deps:[] ~lib_name:None ~scope
  in
  let* modules =
    Modules.singleton_exe module_
    |> Modules.map_user_written ~f:(Pp_spec.pp_module preprocess)
  in
  let dune_version = Scope.project scope |> Dune_project.dune_version in
  let compile_info =
    Lib.DB.resolve_user_written_deps_for_exes (Scope.libs scope)
      [ (t.loc, name) ]
      (Lib_dep.Direct (loc, Lib_name.of_string "cinaps.runtime") :: t.libraries)
      ~pps:(Preprocess.Per_module.pps t.preprocess)
      ~dune_version
  in
  let obj_dir = Obj_dir.make_exe ~dir:cinaps_dir ~name in
  let cctx =
    Compilation_context.create () ~super_context:sctx ~expander ~scope ~obj_dir
      ~modules ~opaque:(Explicit false)
      ~requires_compile:(Lib.Compile.direct_requires compile_info)
      ~requires_link:(Lib.Compile.requires_link compile_info)
      ~flags:(Ocaml_flags.of_list [ "-w"; "-24" ])
      ~js_of_ocaml:None ~package:None
  in
  let* () =
    Exe.build_and_link cctx
      ~program:{ name; main_module_name; loc }
      ~linkages:[ Exe.Linkage.native_or_custom (Super_context.context sctx) ]
      ~promote:None
  in
  let action =
    let open Action_builder.O in
    let module A = Action in
    let cinaps_exe = Path.build cinaps_exe in
    let+ () = Action_builder.path cinaps_exe in
    A.chdir (Path.build dir)
      (A.progn
         (A.run (Ok cinaps_exe) [ "-diff-cmd"; "-" ]
         :: List.map cinapsed_files ~f:(fun fn ->
                A.diff ~optional:true (Path.build fn)
                  (Path.Build.extend_basename fn ~suffix:".cinaps-corrected"))))
  in
  let cinaps_alias = alias ~dir in
  let* () =
    Super_context.add_alias_action sctx ~dir ~loc:(Some loc) cinaps_alias action
  in
  Rules.Produce.Alias.add_deps (Alias.runtest ~dir)
    (Action_builder.alias cinaps_alias)
