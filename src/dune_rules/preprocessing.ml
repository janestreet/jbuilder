open! Dune_engine
open! Stdune
open Import
module SC = Super_context

(* Encoded representation of a set of library names + scope *)
module Key : sig
  (* This module implements a bi-directional function between [encoded] and
     [decoded] *)
  type encoded = Digest.t

  module Decoded : sig
    type t = private
      { pps : Lib_name.t list
      ; project_root : Path.Source.t option
      }

    val of_libs : Lib.t list -> t
  end

  (* [decode y] fails if there hasn't been a previous call to [encode] such that
     [encode x = y]. *)
  val encode : Decoded.t -> encoded

  val decode : encoded -> Decoded.t
end = struct
  type encoded = Digest.t

  module Decoded = struct
    (* Values of type type are preserved in a global table between builds, so
       they must not embed values that are not safe to keep between builds, such
       as [Dune_project.t] values *)
    type t =
      { pps : Lib_name.t list
      ; project_root : Path.Source.t option
      }

    let equal x y =
      List.equal Lib_name.equal x.pps y.pps
      && Option.equal Path.Source.equal x.project_root y.project_root

    let to_string { pps; project_root } =
      let s = String.enumerate_and (List.map pps ~f:Lib_name.to_string) in
      match project_root with
      | None -> s
      | Some dir ->
        sprintf "%s (in project: %s)" s (Path.Source.to_string_maybe_quoted dir)

    let of_libs libs =
      let pps =
        (let compare a b = Lib_name.compare (Lib.name a) (Lib.name b) in
         List.sort libs ~compare)
        |> List.map ~f:Lib.name
      in
      let project =
        List.fold_left libs ~init:None ~f:(fun acc lib ->
            let scope_for_key =
              let info = Lib.info lib in
              let status = Lib_info.status info in
              match status with
              | Private (scope_name, _) -> Some scope_name
              | Installed_private
              | Public _
              | Installed ->
                None
            in
            Option.merge acc scope_for_key ~f:(fun a b ->
                assert (Dune_project.equal a b);
                a))
      in
      { pps; project_root = Option.map project ~f:Dune_project.root }
  end

  (* This mutable table is safe. Even though it can have stale entries remaining
     from previous runs, the entries themselves are correct, so this seems
     harmless apart from the lack of error in [decode] in this situation. *)
  let reverse_table : (Digest.t, Decoded.t) Table.t =
    Table.create (module Digest) 128

  let encode ({ Decoded.pps; project_root } as x) =
    let y = Digest.generic (pps, project_root) in
    match Table.find reverse_table y with
    | None ->
      Table.set reverse_table y x;
      y
    | Some x' ->
      if Decoded.equal x x' then
        y
      else
        User_error.raise
          [ Pp.textf "Hash collision between set of ppx drivers:"
          ; Pp.textf "- cache : %s" (Decoded.to_string x')
          ; Pp.textf "- fetch : %s" (Decoded.to_string x)
          ]

  let decode y =
    match Table.find reverse_table y with
    | Some x -> x
    | None ->
      User_error.raise
        [ Pp.textf "I don't know what ppx rewriters set %s correspond to."
            (Digest.to_string y)
        ]
end

let pped_module m ~f =
  let open Memo.Build.O in
  let pped = Module.pped m in
  let+ () =
    Module.iter m ~f:(fun ml_kind file ->
        let pp_path =
          Module.file pped ~ml_kind |> Option.value_exn
          |> Path.as_in_build_dir_exn
        in
        let file = Path.as_in_build_dir_exn file.path in
        f ml_kind file pp_path)
  in
  pped

module Driver = struct
  module M = struct
    module Info = struct
      let name = Sub_system_name.make "ppx.driver"

      type t =
        { loc : Loc.t
        ; flags : Ordered_set_lang.Unexpanded.t
        ; as_ppx_flags : Ordered_set_lang.Unexpanded.t
        ; lint_flags : Ordered_set_lang.Unexpanded.t
        ; main : string
        ; replaces : (Loc.t * Lib_name.t) list
        }

      type Sub_system_info.t += T of t

      let loc t = t.loc

      (* The syntax of the driver sub-system is part of the main dune syntax, so
         we simply don't create a new one.

         If we wanted to make the ppx system an extension, then we would create
         a new one. *)
      let syntax = Stanza.syntax

      open Dune_lang.Decoder

      let decode =
        fields
          (let+ loc = loc
           and+ flags = Ordered_set_lang.Unexpanded.field "flags"
           and+ as_ppx_flags =
             Ordered_set_lang.Unexpanded.field "as_ppx_flags"
               ~check:(Dune_lang.Syntax.since syntax (1, 2))
           and+ lint_flags = Ordered_set_lang.Unexpanded.field "lint_flags"
           and+ main = field "main" string
           and+ replaces =
             field "replaces" (repeat (located Lib_name.decode)) ~default:[]
           in
           { loc; flags; as_ppx_flags; lint_flags; main; replaces })

      let encode t =
        let open Dune_lang.Encoder in
        let lib (_loc, name) = Lib_name.encode name in
        ( (1, 0)
        , record_fields
          @@ [ field_i "flags" Ordered_set_lang.Unexpanded.encode t.flags
             ; field_i "lint_flags" Ordered_set_lang.Unexpanded.encode
                 t.lint_flags
             ; field "main" string t.main
             ; field_l "replaces" lib t.replaces
             ] )
    end

    type t =
      { info : Info.t
      ; lib : Lib.t
      ; replaces : t list Resolve.t
      }

    let desc ~plural =
      "ppx driver"
      ^
      if plural then
        "s"
      else
        ""

    let desc_article = "a"

    let lib t = t.lib

    let replaces t = t.replaces

    let instantiate ~resolve ~get lib (info : Info.t) =
      let open Memo.Build.O in
      let+ replaces =
        Memo.Build.parallel_map info.replaces ~f:(fun ((loc, name) as x) ->
            Resolve.Build.bind (resolve x) ~f:(fun lib ->
                get ~loc lib >>| function
                | None ->
                  Resolve.fail
                    (User_error.make ~loc
                       [ Pp.textf "%S is not a %s" (Lib_name.to_string name)
                           (desc ~plural:false)
                       ])
                | Some t -> Resolve.return t))
        >>| Resolve.List.map ~f:Fun.id
      in
      { info; lib; replaces }

    let public_info t =
      let open Resolve.O in
      let+ replaces = t.replaces in
      { Info.loc = t.info.loc
      ; flags = t.info.flags
      ; as_ppx_flags = t.info.as_ppx_flags
      ; lint_flags = t.info.lint_flags
      ; main = t.info.main
      ; replaces =
          List.map2 t.info.replaces replaces ~f:(fun (loc, _) t ->
              (loc, Lib.name t.lib))
      }
  end

  include M
  include Sub_system.Register_backend (M)

  (* Where are we called from? *)
  type loc =
    | User_file of Loc.t * (Loc.t * Lib_name.t) list
    | Dot_ppx of Path.Build.t * Lib_name.t list

  let fail loc msg =
    match loc with
    | User_file (loc, _) -> Resolve.fail (User_error.make ~loc [ Pp.text msg ])
    | Dot_ppx (path, pps) ->
      Resolve.fail
        (User_error.make
           ~loc:(Loc.in_file (Path.build path))
           [ Pp.textf "Failed to create on-demand ppx rewriter for %s; %s"
               (String.enumerate_and (List.map pps ~f:Lib_name.to_string))
               (String.uncapitalize msg)
           ])

  let select libs ~loc =
    let open Memo.Build.O in
    select_replaceable_backend libs ~replaces
    >>| Resolve.bind ~f:(function
          | Ok x -> Resolve.return x
          | Error Selection_error.No_backend_found ->
            let msg =
              match
                List.filter_map libs ~f:(fun lib ->
                    match Lib_name.to_string (Lib.name lib) with
                    | ("ocaml-migrate-parsetree" | "ppxlib" | "ppx_driver") as s
                      ->
                      Some s
                    | _ -> None)
              with
              | [] ->
                let pps =
                  match loc with
                  | User_file (_, pps) -> List.map pps ~f:snd
                  | Dot_ppx (_, pps) -> pps
                in
                sprintf
                  "No ppx driver were found. It seems that %s %s not \
                   compatible with Dune. Examples of ppx rewriters that are \
                   compatible with Dune are ones using \
                   ocaml-migrate-parsetree, ppxlib or ppx_driver."
                  (String.enumerate_and (List.map pps ~f:Lib_name.to_string))
                  (match pps with
                  | [ _ ] -> "is"
                  | _ -> "are")
              | names ->
                sprintf
                  "No ppx driver were found.\n\
                   Hint: Try upgrading or reinstalling %s."
                  (String.enumerate_and names)
            in
            fail loc msg
          | Error (Too_many_backends ts) ->
            fail loc
              (sprintf "Too many incompatible ppx drivers were found: %s."
                 (String.enumerate_and
                    (List.map ts ~f:(fun t ->
                         Lib_name.to_string (Lib.name (lib t)))))))
end

let ppx_exe sctx ~key =
  let build_dir = (Super_context.context sctx).build_dir in
  Path.Build.relative build_dir (".ppx/" ^ key ^ "/ppx.exe")

let build_ppx_driver sctx ~scope ~target ~pps ~pp_names =
  let open Memo.Build.O in
  let ctx = SC.context sctx in
  let* driver_and_libs =
    Resolve.Build.bind
      (Resolve.bind pps ~f:(Lib.closure ~linking:true))
      ~f:(fun pps ->
        Driver.select pps ~loc:(Dot_ppx (target, pp_names))
        >>| Resolve.map ~f:(fun driver -> (driver, pps)))
    >>| (* Extend the dependency stack as we don't have locations at this
           point *)
    Resolve.push_stack_frame ~human_readable_description:(fun () ->
        Dyn.pp
          (List [ String "pps"; Dyn.Encoder.(list Lib_name.to_dyn) pp_names ]))
  in
  (* CR-someday diml: what we should do is build the .cmx/.cmo once and for all
     at the point where the driver is defined. *)
  let dir = Path.Build.parent_exn target in
  let main_module_name =
    Module_name.of_string_allow_invalid (Loc.none, "_ppx")
  in
  let module_ = Module.generated ~src_dir:(Path.build dir) main_module_name in
  let ml_source =
    Module.file ~ml_kind:Impl module_
    |> Option.value_exn |> Path.as_in_build_dir_exn
  in
  let add_rule ~sandbox = SC.add_rule ~sandbox sctx ~dir in
  let* () =
    add_rule ~sandbox:Sandbox_config.default
      (Action_builder.write_file_dyn ml_source
         (Resolve.read
            (let open Resolve.O in
            let+ driver, _ = driver_and_libs in
            sprintf "let () = %s ()\n" driver.info.main)))
  in
  let linkages = [ Exe.Linkage.native_or_custom ctx ] in
  let program : Exe.Program.t =
    { name = Filename.remove_extension (Path.Build.basename target)
    ; main_module_name
    ; loc = Loc.none
    }
  in
  let obj_dir = Obj_dir.for_pp ~dir in
  let* cctx =
    let+ expander = Super_context.expander sctx ~dir in
    let requires_compile = Resolve.map driver_and_libs ~f:snd in
    let requires_link = lazy requires_compile in
    let flags = Ocaml_flags.of_list [ "-g"; "-w"; "-24" ] in
    let opaque = Compilation_context.Explicit false in
    let modules = Modules.singleton_exe module_ in
    Compilation_context.create ~super_context:sctx ~scope ~expander ~obj_dir
      ~modules ~flags ~requires_compile ~requires_link ~opaque ~js_of_ocaml:None
      ~package:None ~bin_annot:false ()
  in
  Exe.build_and_link ~program ~linkages cctx ~promote:None

let get_rules sctx key =
  let exe = ppx_exe sctx ~key in
  let pp_names, scope =
    match Digest.from_hex key with
    | None ->
      User_error.raise
        [ Pp.textf "invalid ppx key for %s"
            (Path.Build.to_string_maybe_quoted exe)
        ]
    | Some key ->
      let { Key.Decoded.pps; project_root } = Key.decode key in
      let scope =
        let dir =
          match project_root with
          | None -> (Super_context.context sctx).build_dir
          | Some dir ->
            Path.Build.append_source (Super_context.context sctx).build_dir dir
        in
        Super_context.find_scope_by_dir sctx dir
      in
      (pps, scope)
  in
  let pps =
    let lib_db = Scope.libs scope in
    List.map pp_names ~f:(fun x -> (Loc.none, x)) |> Lib.DB.resolve_pps lib_db
  in
  build_ppx_driver sctx ~scope ~pps ~pp_names ~target:exe

let gen_rules sctx components =
  match components with
  | [ key ] -> get_rules sctx key
  | _ -> Memo.Build.return ()

let ppx_driver_exe sctx libs =
  let key = Digest.to_string (Key.Decoded.of_libs libs |> Key.encode) in
  (* Make sure to compile ppx.exe for the compiling host. See: #2252, #2286 and
     #3698 *)
  let sctx = SC.host sctx in
  ppx_exe sctx ~key

let get_cookies ~loc ~expander ~lib_name libs =
  let open Memo.Build.O in
  let expander, library_name_cookie =
    match lib_name with
    | None -> (expander, None)
    | Some lib_name ->
      let library_name = Lib_name.Local.to_string lib_name in
      let bindings =
        Pform.Map.singleton (Var Library_name) [ Value.String library_name ]
      in
      ( Expander.add_bindings expander ~bindings
      , Some ("library-name", (library_name, Lib_name.of_local (loc, lib_name)))
      )
  in
  let+ cookies =
    Memo.Build.List.concat_map libs ~f:(fun t ->
        let info = Lib.info t in
        let kind = Lib_info.kind info in
        match kind with
        | Normal -> Memo.Build.return []
        | Ppx_rewriter { cookies }
        | Ppx_deriver { cookies } ->
          Memo.Build.List.map
            ~f:(fun { Lib_kind.Ppx_args.Cookie.name; value } ->
              let+ value = Expander.No_deps.expand_str expander value in
              (name, (value, Lib.name t)))
            cookies)
  in
  cookies
  |> (fun l ->
       match library_name_cookie with
       | None -> l
       | Some cookie -> cookie :: l)
  |> String.Map.of_list_reducei
       ~f:(fun name ((val1, lib1) as res) (val2, lib2) ->
         if String.equal val1 val2 then
           res
         else
           let lib1 = Lib_name.to_string lib1 in
           let lib2 = Lib_name.to_string lib2 in
           User_error.raise ~loc
             [ Pp.textf
                 "%s and %s have inconsistent requests for cookie %S; %s \
                  requests %S and %s requests %S"
                 lib1 lib2 name lib1 val1 lib2 val2
             ])
  |> String.Map.to_list_map ~f:(fun name (value, _) ->
         [ "--cookie"; sprintf "%s=%S" name value ])
  |> List.concat

let ppx_driver_and_flags_internal sctx ~loc ~expander ~lib_name ~flags libs =
  let open Action_builder.O in
  let* flags =
    Action_builder.List.map ~f:(Expander.expand_str expander) flags
  in
  let+ cookies =
    Action_builder.memo_build (get_cookies ~loc ~lib_name ~expander libs)
  in
  let sctx = SC.host sctx in
  (ppx_driver_exe sctx libs, flags @ cookies)

let ppx_driver_and_flags sctx ~lib_name ~expander ~scope ~loc ~flags pps =
  let open Action_builder.O in
  let* libs = Resolve.read (Lib.DB.resolve_pps (Scope.libs scope) pps) in
  let* exe, flags =
    ppx_driver_and_flags_internal sctx ~loc ~expander ~lib_name ~flags libs
  in
  let* libs = Resolve.read (Lib.closure libs ~linking:true) in
  let+ driver =
    Action_builder.memo_build (Driver.select libs ~loc:(User_file (loc, pps)))
    >>= Resolve.read
  in
  (exe, driver, flags)

let workspace_root_var =
  String_with_vars.virt_pform __POS__ (Var Workspace_root)

let promote_correction fn build ~suffix =
  let open Action_builder.O in
  let+ act = build in
  Action.progn
    [ act
    ; Action.diff ~optional:true (Path.build fn)
        (Path.Build.extend_basename fn ~suffix)
    ]

let promote_correction_with_target fn build ~suffix =
  Action_builder.progn
    [ build
    ; Action_builder.with_no_targets
        (Action_builder.return
           (Action.diff ~optional:true (Path.build fn)
              (Path.Build.extend_basename fn ~suffix)))
    ]

let chdir action = Action_unexpanded.Chdir (workspace_root_var, action)

let action_for_pp ~loc ~expander ~action ~src =
  let action = chdir action in
  let bindings =
    Pform.Map.singleton (Var Input_file) [ Value.Path (Path.build src) ]
  in
  let expander = Expander.add_bindings expander ~bindings in
  let open Action_builder.O in
  Action_builder.path (Path.build src)
  >>> Action_unexpanded.expand_no_targets action ~loc ~expander ~deps:[]
        ~what:"preprocessing actions"

let action_for_pp_with_target ~loc ~expander ~action ~src ~target =
  let action = action_for_pp ~loc ~expander ~action ~src in
  Action_builder.With_targets.map
    ~f:(Action.with_stdout_to target)
    (Action_builder.with_targets ~targets:[ target ] action)

(* Generate rules for the dialect modules in [modules] and return a a new module
   with only OCaml sources *)
let setup_dialect_rules sctx ~dir ~expander (m : Module.t) =
  let open Memo.Build.O in
  let ml = Module.ml_source m in
  let+ () =
    Module.iter m ~f:(fun ml_kind f ->
        Memo.Build.Option.iter (Dialect.preprocess f.dialect ml_kind)
          ~f:(fun (loc, action) ->
            let src = Path.as_in_build_dir_exn f.path in
            let dst =
              Option.value_exn (Module.file ml ~ml_kind)
              |> Path.as_in_build_dir_exn
            in
            SC.add_rule sctx ~dir
              (action_for_pp_with_target ~loc ~expander ~action ~src ~target:dst)))
  in
  ml

let add_corrected_suffix_binding expander suffix =
  let bindings =
    Pform.Map.singleton (Var Corrected_suffix) [ Value.String suffix ]
  in
  Expander.add_bindings expander ~bindings

let driver_flags expander ~corrected_suffix ~driver_flags ~standard =
  let expander = add_corrected_suffix_binding expander corrected_suffix in
  Expander.expand_and_eval_set expander driver_flags ~standard

let lint_module sctx ~dir ~expander ~lint ~lib_name ~scope =
  let open Action_builder.O in
  Staged.stage
    (let alias = Alias.lint ~dir in
     let add_alias build = SC.add_alias_action sctx alias build ~dir in
     let lint =
       Module_name.Per_item.map lint ~f:(function
         | Preprocess.No_preprocessing ->
           fun ~source:_ ~ast:_ -> Memo.Build.return ()
         | Future_syntax loc ->
           User_error.raise ~loc
             [ Pp.text "'compat' cannot be used as a linter" ]
         | Action (loc, action) ->
           fun ~source ~ast:_ ->
             Module.iter source ~f:(fun _ (src : Module.File.t) ->
                 let src = Path.as_in_build_dir_exn src.path in
                 add_alias ~loc:(Some loc)
                   (action_for_pp ~loc ~expander ~action ~src))
         | Pps { loc; pps; flags; staged } ->
           if staged then
             User_error.raise ~loc
               [ Pp.text "Staged ppx rewriters cannot be used as linters." ];
           let corrected_suffix = ".lint-corrected" in
           let driver_and_flags =
             Action_builder.memoize "ppx driver and flags"
               (let* () = Action_builder.return () in
                let* exe, driver, flags =
                  ppx_driver_and_flags sctx ~expander ~loc ~lib_name ~flags
                    ~scope pps
                in
                let+ ppx_flags =
                  driver_flags expander ~corrected_suffix
                    ~driver_flags:driver.info.lint_flags
                    ~standard:(Action_builder.return [])
                in
                (exe, ppx_flags, flags))
           in
           fun ~source ~ast ->
             Module.iter ast ~f:(fun ml_kind src ->
                 add_alias ~loc:None
                   (promote_correction ~suffix:corrected_suffix
                      (Path.as_in_build_dir_exn
                         (Option.value_exn (Module.file source ~ml_kind)))
                      (let* exe, flags, args = driver_and_flags in
                       let dir =
                         Path.build (Super_context.context sctx).build_dir
                       in
                       Command.run' ~dir
                         (Ok (Path.build exe))
                         [ As args
                         ; Command.Ml_kind.ppx_driver_flag ml_kind
                         ; Dep src.path
                         ; As flags
                         ]))))
     in
     fun ~(source : Module.t) ~ast ->
       Module_name.Per_item.get lint (Module.name source) ~source ~ast)

let make sctx ~dir ~expander ~lint ~preprocess ~preprocessor_deps
    ~instrumentation_deps ~lib_name ~scope =
  let preprocessor_deps = preprocessor_deps @ instrumentation_deps in
  let preprocess =
    Module_name.Per_item.map preprocess ~f:(fun pp ->
        Preprocess.remove_future_syntax ~for_:Compiler pp
          (Super_context.context sctx).version)
  in
  let preprocessor_deps =
    Dep_conf_eval.unnamed preprocessor_deps ~expander
    |> Action_builder.memoize "preprocessor deps"
  in
  let lint_module =
    Staged.unstage (lint_module sctx ~dir ~expander ~lint ~lib_name ~scope)
  in
  let open Action_builder.O in
  Module_name.Per_item.map preprocess ~f:(fun pp ->
      match pp with
      | No_preprocessing ->
        fun m ~lint ->
          let open Memo.Build.O in
          let* ast = setup_dialect_rules sctx ~dir ~expander m in
          let+ () =
            Memo.Build.when_ lint (fun () -> lint_module ~ast ~source:m)
          in
          ast
      | Action (loc, action) ->
        fun m ~lint ->
          let open Memo.Build.O in
          let* ast =
            pped_module m ~f:(fun _kind src dst ->
                let action =
                  action_for_pp_with_target ~loc ~expander ~action ~src
                    ~target:dst
                in
                SC.add_rule sctx ~loc ~dir
                  (let open Action_builder.With_targets.O in
                  Action_builder.with_no_targets preprocessor_deps >>> action))
            >>= setup_dialect_rules sctx ~dir ~expander
          in
          let+ () =
            Memo.Build.when_ lint (fun () -> lint_module ~ast ~source:m)
          in
          ast
      | Pps { loc; pps; flags; staged } ->
        if not staged then
          let corrected_suffix = ".ppx-corrected" in
          let driver_and_flags =
            Action_builder.memoize "ppx driver and flags"
              (let* () = Action_builder.return () in
               let* exe, driver, flags =
                 ppx_driver_and_flags sctx ~expander ~loc ~lib_name ~flags
                   ~scope pps
               in
               let+ ppx_flags =
                 driver_flags expander ~corrected_suffix
                   ~driver_flags:driver.info.flags
                   ~standard:(Action_builder.return [ "--as-ppx" ])
               in
               (exe, ppx_flags, flags))
          in
          fun m ~lint ->
            let open Memo.Build.O in
            let* ast = setup_dialect_rules sctx ~dir ~expander m in
            let* () =
              Memo.Build.when_ lint (fun () -> lint_module ~ast ~source:m)
            in
            pped_module ast ~f:(fun ml_kind src dst ->
                SC.add_rule ~sandbox:Sandbox_config.no_special_requirements sctx
                  ~loc ~dir
                  (promote_correction_with_target ~suffix:corrected_suffix
                     (Path.as_in_build_dir_exn
                        (Option.value_exn (Module.file m ~ml_kind)))
                     (Action_builder.with_targets ~targets:[ dst ]
                        (let open Action_builder.O in
                        preprocessor_deps
                        >>> let* exe, flags, args = driver_and_flags in
                            let dir =
                              Path.build (Super_context.context sctx).build_dir
                            in
                            Command.run' ~dir
                              (Ok (Path.build exe))
                              [ As args
                              ; A "-o"
                              ; Path (Path.build dst)
                              ; Command.Ml_kind.ppx_driver_flag ml_kind
                              ; Dep (Path.build src)
                              ; As flags
                              ]))))
        else
          let dash_ppx_flag =
            Action_builder.memoize "ppx command"
              (let* () = Action_builder.return () in
               let* exe, driver, flags =
                 ppx_driver_and_flags sctx ~expander ~loc ~scope ~flags
                   ~lib_name pps
               in
               let+ () = Action_builder.path (Path.build exe)
               and+ () = preprocessor_deps
               and+ driver_flags =
                 Expander.expand_and_eval_set expander driver.info.as_ppx_flags
                   ~standard:(Action_builder.return [ "--as-ppx" ])
               in
               let driver_flags = driver_flags in
               let command =
                 List.map
                   (List.concat
                      [ [ Path.reach (Path.build exe)
                            ~from:(Path.build (SC.context sctx).build_dir)
                        ]
                      ; driver_flags
                      ; flags
                      ])
                   ~f:String.quote_for_shell
                 |> String.concat ~sep:" "
               in
               [ "-ppx"; command ])
          in
          let pp = Some dash_ppx_flag in
          fun m ~lint ->
            let open Memo.Build.O in
            let* ast = setup_dialect_rules sctx ~dir ~expander m in
            let+ () =
              Memo.Build.when_ lint (fun () -> lint_module ~ast ~source:m)
            in
            Module.set_pp ast pp)
  |> Pp_spec.make

let get_ppx_driver sctx ~loc ~expander ~scope ~lib_name ~flags pps =
  let open Action_builder.O in
  let* libs = Resolve.read (Lib.DB.resolve_pps (Scope.libs scope) pps) in
  ppx_driver_and_flags_internal sctx ~loc ~expander ~lib_name ~flags libs

let ppx_exe sctx ~scope pp =
  let open Resolve.O in
  let+ libs = Lib.DB.resolve_pps (Scope.libs scope) [ (Loc.none, pp) ] in
  ppx_driver_exe sctx libs
