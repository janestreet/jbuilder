open! Dune_engine
open! Stdune
open Import
open Dune_lang.Decoder

(* This file defines Dune types as well as the S-expression syntax for the
   various supported versions of the specification. *)

(* Deprecated *)
module Jbuild_version = struct
  type t = V1

  let decode = enum [ ("1", V1) ]
end

let () =
  Dune_project.Extension.register_deleted ~name:"library_variants"
    ~deleted_in:(2, 6)

module Lint = struct
  type t = Preprocess.Without_instrumentation.t Preprocess.Per_module.t

  let decode = Preprocess.Per_module.decode

  let default = Preprocess.Per_module.default ()

  let no_lint = default
end

module Js_of_ocaml = struct
  type t =
    { flags : Ordered_set_lang.Unexpanded.t
    ; javascript_files : string list
    }

  let decode =
    fields
      (let+ flags = Ordered_set_lang.Unexpanded.field "flags"
       and+ javascript_files =
         field "javascript_files" (repeat string) ~default:[]
       in
       { flags; javascript_files })

  let default =
    { flags = Ordered_set_lang.Unexpanded.standard; javascript_files = [] }
end

type for_ =
  | Executable
  | Library of Wrapped.t option

module Lib_deps = struct
  type t = Lib_dep.t list

  type kind =
    | Required
    | Optional
    | Forbidden

  let decode for_ =
    let+ loc = loc
    and+ t =
      let allow_re_export =
        match for_ with
        | Library _ -> true
        | Executable -> false
      in
      repeat (Lib_dep.decode ~allow_re_export)
    in
    let add kind name acc =
      match Lib_name.Map.find acc name with
      | None -> Lib_name.Map.set acc name kind
      | Some kind' -> (
        match (kind, kind') with
        | Required, Required ->
          User_error.raise ~loc
            [ Pp.textf "library %S is present twice" (Lib_name.to_string name) ]
        | (Optional | Forbidden), (Optional | Forbidden) -> acc
        | Optional, Required
        | Required, Optional ->
          User_error.raise ~loc
            [ Pp.textf
                "library %S is present both as an optional and required \
                 dependency"
                (Lib_name.to_string name)
            ]
        | Forbidden, Required
        | Required, Forbidden ->
          User_error.raise ~loc
            [ Pp.textf
                "library %S is present both as a forbidden and required \
                 dependency"
                (Lib_name.to_string name)
            ])
    in
    ignore
      (List.fold_left t ~init:Lib_name.Map.empty ~f:(fun acc x ->
           match x with
           | Lib_dep.Re_export (_, s)
           | Lib_dep.Direct (_, s) ->
             add Required s acc
           | Select { choices; _ } ->
             List.fold_left choices ~init:acc
               ~f:(fun acc (c : Lib_dep.Select.Choice.t) ->
                 let acc =
                   Lib_name.Set.fold c.required ~init:acc ~f:(add Optional)
                 in
                 Lib_name.Set.fold c.forbidden ~init:acc ~f:(add Forbidden)))
        : kind Lib_name.Map.t);
    t

  let of_pps pps = List.map pps ~f:(fun pp -> Lib_dep.direct (Loc.none, pp))
end

let preprocess_fields =
  let+ preprocess =
    field "preprocess" Preprocess.Per_module.decode
      ~default:(Preprocess.Per_module.default ())
  and+ preprocessor_deps =
    field_o "preprocessor_deps"
      (let+ loc = loc
       and+ l = repeat Dep_conf.decode in
       (loc, l))
  and+ syntax = Dune_lang.Syntax.get_exn Stanza.syntax in
  let preprocessor_deps =
    match preprocessor_deps with
    | None -> []
    | Some (loc, deps) ->
      let deps_might_be_used =
        Module_name.Per_item.exists preprocess ~f:(fun p ->
            match (p : _ Preprocess.t) with
            | Action _
            | Pps _ ->
              true
            | No_preprocessing
            | Future_syntax _ ->
              false)
      in
      if not deps_might_be_used then
        User_warning.emit ~loc
          ~is_error:(syntax >= (2, 0))
          [ Pp.text
              "This preprocessor_deps field will be ignored because no \
               preprocessor that might use them is configured."
          ];
      deps
  in
  (preprocess, preprocessor_deps)

module Buildable = struct
  type t =
    { loc : Loc.t
    ; modules : Ordered_set_lang.t
    ; modules_without_implementation : Ordered_set_lang.t
    ; libraries : Lib_dep.t list
    ; foreign_archives : (Loc.t * Foreign.Archive.t) list
    ; foreign_stubs : Foreign.Stubs.t list
    ; preprocess : Preprocess.With_instrumentation.t Preprocess.Per_module.t
    ; preprocessor_deps : Dep_conf.t list
    ; lint : Preprocess.Without_instrumentation.t Preprocess.Per_module.t
    ; flags : Ocaml_flags.Spec.t
    ; js_of_ocaml : Js_of_ocaml.t
    ; allow_overlapping_dependencies : bool
    ; root_module : (Loc.t * Module_name.t) option
    }

  let decode (for_ : for_) =
    let use_foreign =
      Dune_lang.Syntax.deleted_in Stanza.syntax (2, 0)
        ~extra_info:"Use the (foreign_stubs ...) field instead."
    in
    let only_in_library decode =
      match for_ with
      | Executable -> return None
      | Library _ -> decode
    in
    let add_stubs language ~loc ~names ~flags foreign_stubs =
      match names with
      | None -> foreign_stubs
      | Some names ->
        let names = Ordered_set_lang.replace_standard_with_empty names in
        let flags =
          Option.value ~default:Ordered_set_lang.Unexpanded.standard flags
        in
        Foreign.Stubs.make ~loc ~language ~names ~flags :: foreign_stubs
    in
    let+ loc = loc
    and+ preprocess, preprocessor_deps = preprocess_fields
    and+ lint = field "lint" Lint.decode ~default:Lint.default
    and+ foreign_stubs =
      multi_field "foreign_stubs"
        (Dune_lang.Syntax.since Stanza.syntax (2, 0) >>> Foreign.Stubs.decode)
    and+ foreign_archives =
      field_o "foreign_archives"
        (Dune_lang.Syntax.since Stanza.syntax (2, 0)
        >>> repeat (located Foreign.Archive.decode))
    and+ c_flags =
      only_in_library
        (field_o "c_flags" (use_foreign >>> Ordered_set_lang.Unexpanded.decode))
    and+ cxx_flags =
      only_in_library
        (field_o "cxx_flags"
           (use_foreign >>> Ordered_set_lang.Unexpanded.decode))
    and+ c_names_loc, c_names =
      located
        (only_in_library
           (field_o "c_names" (use_foreign >>> Ordered_set_lang.decode)))
    and+ cxx_names_loc, cxx_names =
      located
        (only_in_library
           (field_o "cxx_names" (use_foreign >>> Ordered_set_lang.decode)))
    and+ modules = Stanza_common.modules_field "modules"
    and+ self_build_stubs_archive_loc, self_build_stubs_archive =
      located
        (only_in_library
           (field ~default:None "self_build_stubs_archive"
              (Dune_lang.Syntax.deleted_in Stanza.syntax (2, 0)
                 ~extra_info:"Use the (foreign_archives ...) field instead."
              >>> enter (maybe string))))
    and+ modules_without_implementation =
      Stanza_common.modules_field "modules_without_implementation"
    and+ libraries = field "libraries" (Lib_deps.decode for_) ~default:[]
    and+ flags = Ocaml_flags.Spec.decode
    and+ js_of_ocaml =
      field "js_of_ocaml" Js_of_ocaml.decode ~default:Js_of_ocaml.default
    and+ allow_overlapping_dependencies =
      field_b "allow_overlapping_dependencies"
    and+ version = Dune_lang.Syntax.get_exn Stanza.syntax
    and+ loc_instrumentation, instrumentation =
      located
        (multi_field "instrumentation"
           (Dune_lang.Syntax.since Stanza.syntax (2, 7)
           >>> fields
                 (let+ backend =
                    field "backend"
                      (let+ libname = located Lib_name.decode
                       and+ flags =
                         let* current_ver =
                           Dune_lang.Syntax.get_exn Stanza.syntax
                         in
                         let version_check flag =
                           let ver = (2, 8) in
                           if current_ver >= ver then
                             flag
                           else
                             let what =
                               "The possibility to pass arguments to \
                                instrumentation backends"
                             in
                             Dune_lang.Syntax.Error.since
                               (String_with_vars.loc flag)
                               Stanza.syntax ver ~what
                         in
                         repeat (String_with_vars.decode >>| version_check)
                       in
                       (libname, flags))
                  and+ deps =
                    field "deps" ~default:[]
                      (Dune_lang.Syntax.since Stanza.syntax (2, 9)
                      >>> repeat Dep_conf.decode)
                  in
                  (backend, deps))))
    and+ root_module =
      field_o "root_module"
        (Dune_lang.Syntax.since Stanza.syntax (2, 8) >>> Module_name.decode_loc)
    in
    let preprocess =
      let init =
        let f libname = Preprocess.With_instrumentation.Ordinary libname in
        Module_name.Per_item.map preprocess ~f:(Preprocess.map ~f)
      in
      List.fold_left instrumentation
        ~f:(fun accu ((backend, flags), deps) ->
          Preprocess.Per_module.add_instrumentation accu
            ~loc:loc_instrumentation ~flags ~deps backend)
        ~init
    in
    let foreign_stubs =
      foreign_stubs
      |> add_stubs C ~loc:c_names_loc ~names:c_names ~flags:c_flags
      |> add_stubs Cxx ~loc:cxx_names_loc ~names:cxx_names ~flags:cxx_flags
    in
    let foreign_archives = Option.value ~default:[] foreign_archives in
    let foreign_archives =
      if
        version < (2, 0)
        && List.is_non_empty foreign_stubs
        && Option.is_some self_build_stubs_archive
      then
        User_error.raise ~loc:self_build_stubs_archive_loc
          [ Pp.concat
              [ Pp.textf "A library cannot use "
              ; Pp.hbox (Pp.textf "(self_build_stubs_archive ...)")
              ; Pp.textf " and "
              ; Pp.hbox (Pp.textf "(c_names ...)")
              ; Pp.textf " simultaneously. This is supported starting from "
              ; Pp.hbox (Pp.textf "Dune 2.0.")
              ]
          ]
      else
        match self_build_stubs_archive with
        | None -> foreign_archives
        (* Note: we add "_stubs" to the name, since [self_build_stubs_archive]
           used this naming convention; [foreign_archives] does not use it and
           allows users to name archives as they like (they still need to add
           the "lib" prefix, however, since standard linkers require it). *)
        | Some name -> (loc, Foreign.Archive.stubs name) :: foreign_archives
    in
    { loc
    ; preprocess
    ; preprocessor_deps
    ; lint
    ; modules
    ; modules_without_implementation
    ; foreign_stubs
    ; foreign_archives
    ; libraries
    ; flags
    ; js_of_ocaml
    ; allow_overlapping_dependencies
    ; root_module
    }

  let has_foreign t =
    List.is_non_empty t.foreign_stubs || List.is_non_empty t.foreign_archives
end

module Public_lib = struct
  type t =
    { name : Loc.t * Lib_name.t
    ; package : Package.t
    ; sub_dir : string option
    }

  let sub_dir t = t.sub_dir

  let loc t = fst t.name

  let name t = snd t.name

  let package t = t.package

  (** if [~allow_deprecated_names] is set, then we allow the package name to be
      attached to one of the deprecated packages *)
  let make ~allow_deprecated_names project ((_, s) as loc_name) =
    let pkg, rest = Lib_name.split s in
    let x =
      if not allow_deprecated_names then
        None
      else
        Dune_project.packages project
        |> Package.Name.Map.values
        |> List.find_map
             ~f:(fun ({ Package.deprecated_package_names; _ } as package) ->
               if Package.Name.Map.mem deprecated_package_names pkg then
                 Some { package; sub_dir = None; name = loc_name }
               else
                 None)
    in
    match x with
    | Some x -> Ok x
    | None ->
      Stanza_common.Pkg.resolve project pkg
      |> Result.map ~f:(fun pkg ->
             { package = pkg
             ; sub_dir =
                 (if rest = [] then
                   None
                 else
                   Some (String.concat rest ~sep:"/"))
             ; name = loc_name
             })

  let decode ~allow_deprecated_names =
    map_validate
      (let+ project = Dune_project.get_exn ()
       and+ loc_name = located Lib_name.decode in
       (project, loc_name))
      ~f:(fun (project, loc_name) ->
        make ~allow_deprecated_names project loc_name)
end

module Mode_conf = struct
  module T = struct
    type t =
      | Byte
      | Native
      | Best

    let compare (a : t) b = Poly.compare a b
  end

  include T

  let decode = enum [ ("byte", Byte); ("native", Native); ("best", Best) ]

  let to_string = function
    | Byte -> "byte"
    | Native -> "native"
    | Best -> "best"

  let to_dyn t =
    let open Dyn.Encoder in
    constr (to_string t) []

  let encode t = Dune_lang.atom (to_string t)

  module Kind = struct
    type t =
      | Inherited
      | Requested of Loc.t
  end

  module Map = struct
    type nonrec 'a t =
      { byte : 'a
      ; native : 'a
      ; best : 'a
      }

    let find t = function
      | Byte -> t.byte
      | Native -> t.native
      | Best -> t.best

    let update t key ~f =
      match key with
      | Byte -> { t with byte = f t.byte }
      | Native -> { t with native = f t.native }
      | Best -> { t with best = f t.best }

    let make_one x = { byte = x; native = x; best = x }
  end

  module Set = struct
    type nonrec t = Kind.t option Map.t

    let empty : t = Map.make_one None

    let of_list (input : (T.t * Kind.t) list) : t =
      List.fold_left ~init:empty input ~f:(fun acc (key, kind) ->
          Map.update acc key ~f:(function
            | None -> Some kind
            | Some (Kind.Requested loc) ->
              User_error.raise ~loc [ Pp.textf "already configured" ]
            | Some Inherited ->
              (* this doesn't happen as inherited can't be manually specified *)
              assert false))

    let decode =
      let decode =
        let+ loc, t = located decode in
        (t, Kind.Requested loc)
      in
      repeat decode >>| of_list

    let default loc : t =
      { empty with byte = Some Inherited; best = Some (Requested loc) }

    module Details = struct
      type t = Kind.t option

      let validate t ~if_ =
        if if_ then
          t
        else
          None

      let ( ||| ) x y =
        if Option.is_some x then
          x
        else
          y
    end

    let eval_detailed t ~has_native =
      let exists = function
        | Best
        | Byte ->
          true
        | Native -> has_native
      in
      let get key : Details.t =
        match Map.find t key with
        | None -> None
        | Some Kind.Inherited -> Option.some_if (exists key) Kind.Inherited
        | Some (Kind.Requested loc) ->
          (* TODO always true for now, but we should delay this error *)
          let exists =
            exists key
            || User_error.raise ~loc [ Pp.text "this mode isn't available" ]
          in
          Option.some_if exists (Kind.Requested loc)
      in
      let best_mode =
        if has_native then
          Native
        else
          Byte
      in
      let best = get Best in
      let open Details in
      let byte = get Byte ||| validate best ~if_:(best_mode = Byte) in
      let native = get Native ||| validate best ~if_:(best_mode = Native) in
      { Mode.Dict.byte; native }

    let eval t ~has_native =
      eval_detailed t ~has_native |> Mode.Dict.map ~f:Option.is_some
  end
end

module Library = struct
  module Wrapped = struct
    include Wrapped

    let default = Simple true

    let make ~wrapped ~implements ~special_builtin_support :
        t Lib_info.Inherited.t =
      (match (wrapped, special_builtin_support) with
      | Some (loc, Yes_with_transition _), Some _ ->
        User_error.raise ~loc
          [ Pp.text
              "Cannot have transition modules for libraries with special \
               builtin support"
          ]
      | _, _ -> ());
      match (wrapped, implements) with
      | None, None -> This default
      | None, Some w -> From w
      | Some (_loc, w), None -> This w
      | Some (loc, _), Some _ ->
        User_error.raise ~loc
          [ Pp.text
              "Wrapped cannot be set for implementations. It is inherited from \
               the virtual library."
          ]

    let field = field_o "wrapped" (located decode)
  end

  type visibility =
    | Public of Public_lib.t
    | Private of Package.t option

  type t =
    { name : Loc.t * Lib_name.Local.t
    ; visibility : visibility
    ; synopsis : string option
    ; install_c_headers : string list
    ; ppx_runtime_libraries : (Loc.t * Lib_name.t) list
    ; modes : Mode_conf.Set.t
    ; kind : Lib_kind.t
    ; library_flags : Ordered_set_lang.Unexpanded.t
    ; c_library_flags : Ordered_set_lang.Unexpanded.t
    ; virtual_deps : (Loc.t * Lib_name.t) list
    ; wrapped : Wrapped.t Lib_info.Inherited.t
    ; optional : bool
    ; buildable : Buildable.t
    ; dynlink : Dynlink_supported.t
    ; project : Dune_project.t
    ; sub_systems : Sub_system_info.t Sub_system_name.Map.t
    ; dune_version : Dune_lang.Syntax.Version.t
    ; virtual_modules : Ordered_set_lang.t option
    ; implements : (Loc.t * Lib_name.t) option
    ; default_implementation : (Loc.t * Lib_name.t) option
    ; private_modules : Ordered_set_lang.t option
    ; stdlib : Ocaml_stdlib.t option
    ; special_builtin_support : Lib_info.Special_builtin_support.t option
    ; enabled_if : Blang.t
    ; instrumentation_backend : (Loc.t * Lib_name.t) option
    }

  let decode =
    fields
      (let* stanza_loc = loc in
       let* wrapped = Wrapped.field in
       let* dune_version = Dune_lang.Syntax.get_exn Stanza.syntax in
       let+ buildable = Buildable.decode (Library (Option.map ~f:snd wrapped))
       and+ name = field_o "name" Lib_name.Local.decode_loc
       and+ public =
         field_o "public_name" (Public_lib.decode ~allow_deprecated_names:false)
       and+ synopsis = field_o "synopsis" string
       and+ install_c_headers =
         field "install_c_headers" (repeat string) ~default:[]
       and+ ppx_runtime_libraries =
         field "ppx_runtime_libraries"
           (repeat (located Lib_name.decode))
           ~default:[]
       and+ library_flags = Ordered_set_lang.Unexpanded.field "library_flags"
       and+ c_library_flags =
         Ordered_set_lang.Unexpanded.field "c_library_flags"
       and+ virtual_deps =
         field "virtual_deps" (repeat (located Lib_name.decode)) ~default:[]
       and+ modes =
         field "modes" Mode_conf.Set.decode
           ~default:(Mode_conf.Set.default stanza_loc)
       and+ kind = field "kind" Lib_kind.decode ~default:Lib_kind.Normal
       and+ optional = field_b "optional"
       and+ no_dynlink = field_b "no_dynlink"
       and+ () =
         let check =
           let+ loc = loc in
           let is_error = dune_version >= (2, 0) in
           User_warning.emit ~loc ~is_error
             [ Pp.text "no_keep_locs is a no-op. Please delete it." ]
         in
         let+ _ = field_b "no_keep_locs" ~check in
         ()
       and+ sub_systems =
         let* () = return () in
         Sub_system_info.record_parser ()
       and+ project = Dune_project.get_exn ()
       and+ virtual_modules =
         field_o "virtual_modules"
           (Dune_lang.Syntax.since Stanza.syntax (1, 7)
           >>> Ordered_set_lang.decode)
       and+ implements =
         field_o "implements"
           (Dune_lang.Syntax.since Stanza.syntax (1, 7)
           >>> located Lib_name.decode)
       and+ default_implementation =
         field_o "default_implementation"
           (Dune_lang.Syntax.since Stanza.syntax (2, 6)
           >>> located Lib_name.decode)
       and+ private_modules =
         field_o "private_modules"
           (let* () = Dune_lang.Syntax.since Stanza.syntax (1, 2) in
            Ordered_set_lang.decode)
       and+ stdlib =
         field_o "stdlib"
           (Dune_lang.Syntax.since Ocaml_stdlib.syntax (0, 1)
           >>> Ocaml_stdlib.decode)
       and+ special_builtin_support =
         field_o "special_builtin_support"
           (Dune_lang.Syntax.since Stanza.syntax (1, 10)
           >>> Lib_info.Special_builtin_support.decode)
       and+ enabled_if =
         let open Enabled_if in
         let allowed_vars = Only Lib_config.allowed_in_enabled_if in
         decode ~allowed_vars ~since:(Some (1, 10)) ()
       and+ instrumentation_backend =
         field_o "instrumentation.backend"
           (Dune_lang.Syntax.since Stanza.syntax (2, 7)
           >>> fields (field "ppx" (located Lib_name.decode)))
       and+ package =
         field_o "package"
           (Dune_lang.Syntax.since Stanza.syntax (2, 8)
           >>> located Stanza_common.Pkg.decode)
       in
       let wrapped =
         Wrapped.make ~wrapped ~implements ~special_builtin_support
       in
       let name =
         let open Dune_lang.Syntax.Version.Infix in
         match (name, public) with
         | Some (loc, res), _ -> (loc, res)
         | None, Some { name = loc, name; _ } ->
           if dune_version >= (1, 1) then
             match Lib_name.to_local (loc, name) with
             | Ok m -> (loc, m)
             | Error user_message ->
               User_error.raise ~loc
                 [ Pp.textf "Invalid library name."
                 ; Pp.text
                     "Public library names don't have this restriction. You \
                      can either change this public name to be a valid library \
                      name or add a \"name\" field with a valid library name."
                 ]
                 ~hints:(Lib_name.Local.valid_format_doc :: user_message.hints)
           else
             User_error.raise ~loc
               [ Pp.text
                   "name field cannot be omitted before version 1.1 of the \
                    dune language"
               ]
         | None, None ->
           User_error.raise ~loc:stanza_loc
             [ Pp.text
                 (if dune_version >= (1, 1) then
                   "supply at least one of name or public_name fields"
                 else
                   "name field is missing")
             ]
       in
       let visibility =
         match (public, package) with
         | None, None -> Private None
         | Some public, None -> Public public
         | None, Some (_loc, package) -> Private (Some package)
         | Some public, Some (loc, _) ->
           User_error.raise ~loc
             [ Pp.textf
                 "This library has a public_name, it already belongs to the \
                  package %s"
                 (Package.Name.to_string (Package.name public.package))
             ]
       in
       Option.both virtual_modules implements
       |> Option.iter ~f:(fun (virtual_modules, (_, impl)) ->
              User_error.raise
                ~loc:(Ordered_set_lang.loc virtual_modules |> Option.value_exn)
                [ Pp.textf "A library cannot be both virtual and implement %s"
                    (Lib_name.to_string impl)
                ]);
       (match (virtual_modules, default_implementation) with
       | None, Some (loc, _) ->
         User_error.raise ~loc
           [ Pp.text
               "Only virtual libraries can specify a default implementation."
           ]
       | _ -> ());
       { name
       ; visibility
       ; synopsis
       ; install_c_headers
       ; ppx_runtime_libraries
       ; modes
       ; kind
       ; library_flags
       ; c_library_flags
       ; virtual_deps
       ; wrapped
       ; optional
       ; buildable
       ; dynlink = Dynlink_supported.of_bool (not no_dynlink)
       ; project
       ; sub_systems
       ; dune_version
       ; virtual_modules
       ; implements
       ; default_implementation
       ; private_modules
       ; stdlib
       ; special_builtin_support
       ; enabled_if
       ; instrumentation_backend
       })

  let package t =
    match t.visibility with
    | Public p -> Some p.package
    | Private p -> p

  let sub_dir t =
    match t.visibility with
    | Public p -> p.sub_dir
    | Private None -> None
    | Private (Some _) ->
      Lib_name.Local.mangled_path_under_package (snd t.name)
      |> String.concat ~sep:"/" |> Option.some

  let has_foreign t = Buildable.has_foreign t.buildable

  let foreign_archives t =
    (if List.is_empty t.buildable.foreign_stubs then
      []
    else
      [ Foreign.Archive.stubs (Lib_name.Local.to_string (snd t.name)) ])
    @ List.map ~f:snd t.buildable.foreign_archives

  let foreign_lib_files t ~dir ~ext_lib =
    List.map (foreign_archives t) ~f:(fun archive ->
        Foreign.Archive.lib_file ~archive ~dir ~ext_lib)

  let foreign_dll_files t ~dir ~ext_dll =
    List.map (foreign_archives t) ~f:(fun archive ->
        Foreign.Archive.dll_file ~archive ~dir ~ext_dll)

  let archive_basename t ~ext = Lib_name.Local.to_string (snd t.name) ^ ext

  let archive t ~dir ~ext = Path.Build.relative dir (archive_basename t ~ext)

  let best_name t =
    match t.visibility with
    | Private _ -> Lib_name.of_local t.name
    | Public p -> snd p.name

  let is_virtual t = Option.is_some t.virtual_modules

  let is_impl t = Option.is_some t.implements

  let obj_dir ~dir t =
    let private_lib =
      match t.visibility with
      | Private (Some _) -> true
      | Private None
      | Public _ ->
        false
    in
    Obj_dir.make_lib ~dir
      ~has_private_modules:
        ((* TODO instead of this fragile approximation, we should be looking at
            [Modules.t] and deciding. Unfortunately, [Obj_dir.t] is currently
            used in some places where [Modules.t] is not yet constructed. *)
         t.private_modules <> None
        || t.buildable.root_module <> None)
      ~private_lib (snd t.name)

  let main_module_name t : Lib_info.Main_module_name.t =
    match (t.implements, t.wrapped) with
    | Some x, From _ -> From x
    | Some _, This _ (* cannot specify for wrapped for implements *)
    | None, From _ ->
      assert false (* cannot inherit for normal libs *)
    | None, This (Simple false) -> This None
    | None, This (Simple true | Yes_with_transition _) ->
      This (Some (Module_name.of_local_lib_name (snd t.name)))

  let to_lib_info conf ~dir
      ~lib_config:
        ({ Lib_config.has_native; ext_lib; ext_dll; natdynlink_supported; _ } as
        lib_config) =
    let open Memo.Build.O in
    let obj_dir = obj_dir ~dir conf in
    let archive ?(dir = dir) ext = archive conf ~dir ~ext in
    let modes = Mode_conf.Set.eval ~has_native conf.modes in
    let archive_for_mode ~f_ext ~mode =
      if Mode.Dict.get modes mode then
        Some (archive (f_ext mode))
      else
        None
    in
    let archives_for_mode ~f_ext =
      Mode.Dict.of_func (fun ~mode ->
          archive_for_mode ~f_ext ~mode |> Option.to_list)
    in
    let jsoo_runtime =
      List.map conf.buildable.js_of_ocaml.javascript_files
        ~f:(Path.Build.relative dir)
    in
    let status =
      match conf.visibility with
      | Private pkg -> Lib_info.Status.Private (conf.project, pkg)
      | Public p -> Public (conf.project, p.package)
    in
    let virtual_library = is_virtual conf in
    let foreign_archives = foreign_lib_files conf ~dir ~ext_lib in
    let native_archives =
      let archive = archive ext_lib in
      if virtual_library || not modes.native then
        Lib_info.Files []
      else if
        Option.is_some conf.implements
        || Lib_config.linker_can_create_empty_archives lib_config
           && Ocaml_version.ocamlopt_always_calls_library_linker
                lib_config.ocaml_version
      then
        Lib_info.Files [ archive ]
      else
        Lib_info.Needs_module_info archive
    in
    let foreign_dll_files = foreign_dll_files conf ~dir ~ext_dll in
    let exit_module = Option.bind conf.stdlib ~f:(fun x -> x.exit_module) in
    let jsoo_archive =
      (* XXX we shouldn't access the directory of the obj_dir directly. We
         should use something like [Obj_dir.Archive.obj] instead *)
      if modes.byte then
        Some (archive ~dir:(Obj_dir.obj_dir obj_dir) ".cma.js")
      else
        None
    in
    let virtual_ =
      Option.map conf.virtual_modules ~f:(fun _ -> Lib_info.Source.Local)
    in
    let foreign_objects = Lib_info.Source.Local in
    let archives, plugins =
      if virtual_library then
        (Mode.Dict.make_both [], Mode.Dict.make_both [])
      else
        let plugins =
          let archive_file ~mode =
            archive_for_mode ~f_ext:Mode.plugin_ext ~mode |> Option.to_list
          in
          { Mode.Dict.native =
              (if Dynlink_supported.get conf.dynlink natdynlink_supported then
                archive_file ~mode:Native
              else
                [])
          ; byte = archive_file ~mode:Byte
          }
        in
        (archives_for_mode ~f_ext:Mode.compiled_lib_ext, plugins)
    in
    let main_module_name = main_module_name conf in
    let name = best_name conf in
    let+ enabled =
      let+ enabled_if_result =
        Blang.eval conf.enabled_if ~dir:(Path.build dir)
          ~f:(fun ~source:_ pform ->
            let value = Lib_config.get_for_enabled_if lib_config pform in
            Memo.Build.return [ Value.String value ])
      in
      if not enabled_if_result then
        Lib_info.Enabled_status.Disabled_because_of_enabled_if
      else if conf.optional then
        Optional
      else
        Normal
    in
    let version =
      match status with
      | Public (_, pkg) -> pkg.version
      | Installed_private
      | Installed
      | Private _ ->
        None
    in
    let requires = conf.buildable.libraries in
    let loc = conf.buildable.loc in
    let kind = conf.kind in
    let src_dir = dir in
    let orig_src_dir = None in
    let synopsis = conf.synopsis in
    let sub_systems = conf.sub_systems in
    let ppx_runtime_deps = conf.ppx_runtime_libraries in
    let preprocess = conf.buildable.preprocess in
    let virtual_deps = conf.virtual_deps in
    let dune_version = Some conf.dune_version in
    let implements = conf.implements in
    let default_implementation = conf.default_implementation in
    let wrapped = Some conf.wrapped in
    let special_builtin_support = conf.special_builtin_support in
    let instrumentation_backend = conf.instrumentation_backend in
    let entry_modules = Lib_info.Source.Local in
    Lib_info.create ~loc ~path_kind:Local ~name ~kind ~status ~src_dir
      ~orig_src_dir ~obj_dir ~version ~synopsis ~main_module_name ~sub_systems
      ~requires ~foreign_objects ~plugins ~archives ~ppx_runtime_deps
      ~foreign_archives ~native_archives ~foreign_dll_files ~jsoo_runtime
      ~jsoo_archive ~preprocess ~enabled ~virtual_deps ~dune_version ~virtual_
      ~entry_modules ~implements ~default_implementation ~modes ~wrapped
      ~special_builtin_support ~exit_module ~instrumentation_backend
end

module Plugin = struct
  type t =
    { package : Package.t
    ; name : Package.Name.t
    ; libraries : (Loc.t * Lib_name.t) list
    ; site : Loc.t * (Package.Name.t * Section.Site.t)
    ; optional : bool
    }

  let decode =
    fields
      (let+ name = field "name" Package.Name.decode
       and+ libraries = field "libraries" (repeat (located Lib_name.decode))
       and+ site =
         field "site" (located (pair Package.Name.decode Section.Site.decode))
       and+ package = Stanza_common.Pkg.field ~stanza:"plugin"
       and+ optional = field_b "optional" in
       { name; libraries; site; package; optional })
end

module Install_conf = struct
  type t =
    { section : Install.Section_with_site.t
    ; files : File_binding.Unexpanded.t list
    ; package : Package.t
    ; enabled_if : Blang.t
    }

  let decode =
    fields
      (let+ section = field "section" Install.Section_with_site.decode
       and+ files = field "files" File_binding.Unexpanded.L.decode
       and+ package = Stanza_common.Pkg.field ~stanza:"install"
       and+ enabled_if =
         let allowed_vars = Enabled_if.common_vars ~since:(2, 6) in
         Enabled_if.decode ~allowed_vars ~since:(Some (2, 6)) ()
       in
       { section; files; package; enabled_if })
end

module Promote = struct
  let into_decode =
    let+ loc, dir = located relative_file in
    { Rule.Promote.Into.loc; dir }

  let decode : Rule.Promote.t Dune_lang.Decoder.t =
    fields
      (let+ until_clean =
         field_b "until-clean"
           ~check:(Dune_lang.Syntax.since Stanza.syntax (1, 10))
       and+ into =
         field_o "into"
           (Dune_lang.Syntax.since Stanza.syntax (1, 10) >>> into_decode)
       and+ only =
         field_o "only"
           (Dune_lang.Syntax.since Stanza.syntax (1, 10)
           >>> Predicate_lang.Glob.decode)
       in
       { Rule.Promote.lifetime =
           (if until_clean then
             Until_clean
           else
             Unlimited)
       ; into
       ; only
       })
end

module Executables = struct
  module Names : sig
    type t

    val names : t -> (Loc.t * string) list

    val package : t -> Package.t option

    val has_public_name : t -> bool

    val make :
         multi:bool
      -> stanza:string
      -> allow_omit_names_version:Dune_lang.Syntax.Version.t
      -> (t, fields) Dune_lang.Decoder.parser

    val install_conf :
      t -> ext:string -> enabled_if:Blang.t -> Install_conf.t option
  end = struct
    type public =
      { public_names : (Loc.t * string option) list
      ; package : Package.t
      }

    type t =
      { names : (Loc.t * string) list
      ; public : public option
      ; stanza : string
      ; project : Dune_project.t
      ; loc : Loc.t
      ; multi : bool
      }

    let names t = t.names

    let package t = Option.map t.public ~f:(fun p -> p.package)

    let has_public_name t = Option.is_some t.public

    let public_name =
      located string >>| fun (loc, s) ->
      ( loc
      , match s with
        | "-" -> None
        | s -> Some s )

    let multi_fields =
      map_validate
        (let+ names = field_o "names" (repeat1 (located string))
         and+ pub_names = field_o "public_names" (repeat1 public_name) in
         (names, pub_names))
        ~f:(fun (names, public_names) ->
          match (names, public_names) with
          | Some names, Some public_names ->
            if List.length public_names = List.length names then
              Ok (Some names, Some public_names)
            else
              Error
                (User_error.make
                   [ Pp.text
                       "The list of public names must be of the same length as \
                        the list of names"
                   ])
          | names, public_names -> Ok (names, public_names))

    let single_fields =
      let+ name = field_o "name" (located string)
      and+ public_name = field_o "public_name" (located string) in
      ( Option.map name ~f:List.singleton
      , Option.map public_name ~f:(fun (loc, s) -> [ (loc, Some s) ]) )

    let pluralize s ~multi =
      if multi then
        s ^ "s"
      else
        s

    let make ~multi ~stanza ~allow_omit_names_version =
      let check_valid_name_version = (3, 0) in
      let+ names =
        if multi then
          multi_fields
        else
          single_fields
      and+ loc = loc
      and+ dune_syntax = Dune_lang.Syntax.get_exn Stanza.syntax
      and+ package =
        field_o "package"
          (let+ loc = loc
           and+ pkg = Stanza_common.Pkg.decode in
           (loc, pkg))
      and+ project = Dune_project.get_exn () in
      let names, public_names = names in
      let stanza = pluralize stanza ~multi in
      let names =
        let open Dune_lang.Syntax.Version.Infix in
        if dune_syntax >= check_valid_name_version then
          Option.iter names
            ~f:
              (List.iter ~f:(fun name ->
                   ignore (Module_name.parse_string_exn name : Module_name.t)));
        match (names, public_names) with
        | Some names, _ -> names
        | None, Some public_names ->
          if dune_syntax >= allow_omit_names_version then
            let check_names = dune_syntax >= check_valid_name_version in
            List.map public_names ~f:(fun (loc, p) ->
                match (p, check_names) with
                | None, _ ->
                  User_error.raise ~loc
                    [ Pp.text "This executable must have a name field" ]
                | Some s, false -> (loc, s)
                | Some s, true -> (
                  match Module_name.of_string_user_error (loc, s) with
                  | Ok _ -> (loc, s)
                  | Error user_message ->
                    User_error.raise ~loc
                      [ Pp.textf "Invalid module name."
                      ; Pp.text
                          "Public executable names don't have this \
                           restriction. You can either change this public name \
                           to be a valid module name or add a \"name\" field \
                           with a valid module name."
                      ]
                      ~hints:(Module_name.valid_format_doc :: user_message.hints)
                  ))
          else
            User_error.raise ~loc
              [ Pp.textf "%s field may not be omitted before dune version %s"
                  (pluralize ~multi "name")
                  (Dune_lang.Syntax.Version.to_string allow_omit_names_version)
              ]
        | None, None ->
          if dune_syntax >= allow_omit_names_version then
            User_error.raise ~loc
              [ Pp.textf "either the %s or the %s field must be present"
                  (pluralize ~multi "name")
                  (pluralize ~multi "public_name")
              ]
          else
            User_error.raise ~loc
              [ Pp.textf "field %s is missing" (pluralize ~multi "name") ]
      in
      let public =
        match (package, public_names) with
        | None, None -> None
        | Some (_loc, package), Some public_names ->
          Some { package; public_names }
        | None, Some public_names ->
          if List.for_all public_names ~f:(fun (_, x) -> Option.is_none x) then
            None
          else
            Some
              { public_names
              ; package =
                  Stanza_common.Pkg.default_exn ~loc project
                    (pluralize "executable" ~multi)
              }
        | Some (loc, _), None ->
          User_error.raise ~loc
            [ Pp.textf "This field is useless without a (%s ...) field."
                (pluralize "public_name" ~multi)
            ]
      in
      { names; public; project; stanza; loc; multi }

    let install_conf t ~ext ~enabled_if =
      Option.map t.public ~f:(fun { package; public_names } ->
          let files =
            List.map2 t.names public_names ~f:(fun (locn, name) (locp, pub) ->
                Option.map pub ~f:(fun pub ->
                    File_binding.Unexpanded.make
                      ~src:(locn, name ^ ext)
                      ~dst:(locp, pub)))
            |> List.filter_opt
          in
          { Install_conf.section = Section Bin; files; package; enabled_if })
  end

  module Link_mode = struct
    module T = struct
      type t =
        | Byte_complete
        | Other of
            { mode : Mode_conf.t
            ; kind : Binary_kind.t
            }

      let compare a b =
        match (a, b) with
        | Byte_complete, Byte_complete -> Eq
        | Byte_complete, _ -> Lt
        | _, Byte_complete -> Gt
        | Other a, Other b -> (
          match Poly.compare a.mode b.mode with
          | Eq -> Poly.compare a.kind b.kind
          | ne -> ne)

      let to_dyn _ = Dyn.opaque
    end

    include T

    let make mode kind = Other { mode; kind }

    let exe = make Best Exe

    let object_ = make Best Object

    let shared_object = make Best Shared_object

    let byte = make Byte Exe

    let native = make Native Exe

    let js = make Byte Js

    let plugin = make Best Plugin

    let installable_modes = [ exe; native; byte ]

    let simple_representations =
      [ ("exe", exe)
      ; ("object", object_)
      ; ("shared_object", shared_object)
      ; ("byte", byte)
      ; ("native", native)
      ; ("js", js)
      ; ("byte_complete", Byte_complete)
      ; ("plugin", plugin)
      ]

    let simple = Dune_lang.Decoder.enum simple_representations

    let decode =
      enter
        (let+ mode = Mode_conf.decode
         and+ kind = Binary_kind.decode in
         make mode kind)
      <|> simple

    let simple_encode link_mode =
      let is_ok (_, candidate) = compare candidate link_mode = Eq in
      List.find ~f:is_ok simple_representations
      |> Option.map ~f:(fun (s, _) -> Dune_lang.atom s)

    let encode link_mode =
      match simple_encode link_mode with
      | Some s -> s
      | None -> (
        match link_mode with
        | Byte_complete -> assert false
        | Other { mode; kind } ->
          Dune_lang.Encoder.pair Mode_conf.encode Binary_kind.encode (mode, kind)
        )

    let to_dyn t =
      match t with
      | Byte_complete -> Dyn.Variant ("Byte_complete", [])
      | Other { mode; kind } ->
        let open Dyn.Encoder in
        Variant
          ( "Other"
          , [ record
                [ ("mode", Mode_conf.to_dyn mode)
                ; ("kind", Binary_kind.to_dyn kind)
                ]
            ] )

    let extension t ~loc ~ext_obj ~ext_dll =
      match t with
      | Byte_complete -> ".bc.exe"
      | Other { mode; kind } -> (
        let same_as_mode : Mode.t =
          match mode with
          | Byte -> Byte
          | Native
          | Best ->
            (* From the point of view of the extension, [native] and [best] are
               the same *)
            Native
        in
        match (same_as_mode, kind) with
        | Byte, C -> ".bc.c"
        | Native, C ->
          User_error.raise ~loc
            [ Pp.text "C file generation only supports bytecode!" ]
        | Byte, Exe -> ".bc"
        | Native, Exe -> ".exe"
        | Byte, Object -> ".bc" ^ ext_obj
        | Native, Object -> ".exe" ^ ext_obj
        | Byte, Shared_object -> ".bc" ^ ext_dll
        | Native, Shared_object -> ext_dll
        | mode, Plugin -> Mode.plugin_ext mode
        | Byte, Js -> ".bc.js"
        | Native, Js ->
          User_error.raise ~loc
            [ Pp.text "Javascript generation only supports bytecode!" ])

    module O = Comparable.Make (T)

    module Map = struct
      include O.Map

      let decode =
        located (repeat (located decode)) >>| fun (loc, l) ->
        match l with
        | [] -> User_error.raise ~loc [ Pp.textf "No linking mode defined" ]
        | l ->
          let t =
            List.fold_left l ~init:empty ~f:(fun acc (loc, link_mode) ->
                set acc link_mode loc)
          in
          (match
             String.Map.of_list_map (to_list t) ~f:(fun (lm, loc) ->
                 (extension lm ~loc ~ext_obj:".OBJ" ~ext_dll:".DLL", lm))
           with
          | Ok _ -> ()
          | Error (_ext, (lm1, _), (lm2, _)) ->
            User_error.raise ~loc
              [ Pp.textf
                  "It is not allowed use both %s and %s together as they use \
                   the same file extension."
                  (Dune_lang.to_string (encode lm1))
                  (Dune_lang.to_string (encode lm2))
              ]);
          t

      let byte_and_exe = of_list_exn [ (byte, Loc.none); (exe, Loc.none) ]

      let default_for_exes ~version =
        if version < (2, 0) then
          byte_and_exe
        else
          singleton exe Loc.none

      let default_for_tests = byte_and_exe

      let best_install_mode t = List.find ~f:(mem t) installable_modes
    end
  end

  type t =
    { names : (Loc.t * string) list
    ; link_flags : Ordered_set_lang.Unexpanded.t
    ; link_deps : Dep_conf.t list
    ; modes : Loc.t Link_mode.Map.t
    ; optional : bool
    ; buildable : Buildable.t
    ; package : Package.t option
    ; promote : Rule.Promote.t option
    ; install_conf : Install_conf.t option
    ; embed_in_plugin_libraries : (Loc.t * Lib_name.t) list
    ; forbidden_libraries : (Loc.t * Lib_name.t) list
    ; bootstrap_info : string option
    ; enabled_if : Blang.t
    }

  let bootstrap_info_extension =
    let syntax =
      Dune_lang.Syntax.create ~name:"dune-bootstrap-info"
        ~desc:"private extension to handle Dune bootstrap"
        [ ((0, 1), `Since (2, 0)) ]
    in
    Dune_project.Extension.register syntax (return ((), [])) Dyn.Encoder.unit

  let common =
    let* dune_version = Dune_lang.Syntax.get_exn Stanza.syntax in
    let+ buildable = Buildable.decode Executable
    and+ (_ : bool) =
      field "link_executables" ~default:true
        (Dune_lang.Syntax.deleted_in Stanza.syntax (1, 0) >>> bool)
    and+ link_deps = field "link_deps" (repeat Dep_conf.decode) ~default:[]
    and+ link_flags = Ordered_set_lang.Unexpanded.field "link_flags"
    and+ modes =
      field "modes" Link_mode.Map.decode
        ~default:(Link_mode.Map.default_for_exes ~version:dune_version)
    and+ optional =
      field_b "optional" ~check:(Dune_lang.Syntax.since Stanza.syntax (2, 0))
    and+ promote =
      field_o "promote"
        (Dune_lang.Syntax.since Stanza.syntax (1, 11) >>> Promote.decode)
    and+ () =
      map_validate
        (field "inline_tests" (repeat junk >>| fun _ -> true) ~default:false)
        ~f:(function
          | false -> Ok ()
          | true ->
            Error
              (User_error.make
                 [ Pp.text "Inline tests are only allowed in libraries."
                 ; Pp.text
                     "See https://github.com/ocaml/dune/issues/745 for more \
                      details."
                 ]))
    and+ embed_in_plugin_libraries =
      field_o "embed_in_plugin_libraries"
        (Dune_lang.Syntax.since Stanza.syntax (2, 4)
        >>> located (repeat (located Lib_name.decode)))
    and+ forbidden_libraries =
      field "forbidden_libraries"
        (Dune_lang.Syntax.since Stanza.syntax (2, 0)
        >>> repeat (located Lib_name.decode))
        ~default:[]
    and+ bootstrap_info =
      field_o "bootstrap_info"
        (let+ loc = loc
         and+ fname = filename
         and+ project = Dune_project.get_exn () in
         if
           Option.is_none
             (Dune_project.find_extension_args project bootstrap_info_extension)
         then
           User_error.raise ~loc
             [ Pp.text "This field is reserved for Dune itself" ];
         fname)
    and+ enabled_if =
      let allowed_vars = Enabled_if.common_vars ~since:(2, 3) in
      let* syntax_version = Dune_lang.Syntax.get_exn Stanza.syntax in
      let is_error =
        Dune_lang.Syntax.Version.Infix.(syntax_version >= (2, 6))
      in
      Enabled_if.decode ~allowed_vars ~is_error ~since:(Some (2, 3)) ()
    in
    fun names ~multi ->
      let has_public_name = Names.has_public_name names in
      let private_names = Names.names names in
      let install_conf =
        match Link_mode.Map.best_install_mode modes with
        | None when has_public_name ->
          User_error.raise ~loc:buildable.loc
            [ Pp.textf "No installable mode found for %s."
                (if multi then
                  "these executables"
                else
                  "this executable")
            ; Pp.text
                "When public_name is set, one of the following modes is \
                 required:"
            ; Pp.enumerate Link_mode.installable_modes ~f:(fun mode ->
                  Pp.verbatim (Dune_lang.to_string (Link_mode.encode mode)))
            ]
        | None -> None
        | Some mode ->
          let ext =
            match mode with
            | Byte_complete
            | Other { mode = Byte; _ } ->
              ".bc"
            | Other { mode = Native | Best; _ } -> ".exe"
          in
          Names.install_conf names ~ext ~enabled_if
      in
      let embed_in_plugin_libraries =
        let plugin =
          Link_mode.Map.existsi modes ~f:(fun mode _ ->
              match mode with
              | Link_mode.Other { kind = Plugin; _ } -> true
              | _ -> false)
        in
        match (embed_in_plugin_libraries, plugin) with
        | None, _ -> []
        | Some (_, l), true -> l
        | Some (loc, _), false ->
          User_error.raise ~loc
            [ Pp.textf "This field can only be used when linking a plugin." ]
      in
      { names = private_names
      ; link_flags
      ; link_deps
      ; modes
      ; optional
      ; buildable
      ; package = Names.package names
      ; promote
      ; install_conf
      ; embed_in_plugin_libraries
      ; forbidden_libraries
      ; bootstrap_info
      ; enabled_if
      }

  let single, multi =
    let stanza = "executable" in
    let make multi =
      fields
        (let+ names = Names.make ~multi ~stanza ~allow_omit_names_version:(1, 1)
         and+ f = common in
         f names ~multi)
    in
    (make false, make true)

  let has_foreign t = Buildable.has_foreign t.buildable

  let obj_dir t ~dir = Obj_dir.make_exe ~dir ~name:(snd (List.hd t.names))
end

module Rule = struct
  module Mode = struct
    include Rule.Mode

    let decode =
      let promote_into lifetime =
        let+ () = Dune_lang.Syntax.since Stanza.syntax (1, 8)
        and+ into = Promote.into_decode in
        Rule.Mode.Promote { lifetime; into = Some into; only = None }
      in
      sum
        [ ("standard", return Rule.Mode.Standard)
        ; ("fallback", return Rule.Mode.Fallback)
        ; ( "promote"
          , let+ p = Promote.decode in
            Rule.Mode.Promote p )
        ; ( "promote-until-clean"
          , return
              (Rule.Mode.Promote
                 { lifetime = Until_clean; into = None; only = None }) )
        ; ("promote-into", promote_into Unlimited)
        ; ("promote-until-clean-into", promote_into Until_clean)
        ]

    let field = field "mode" decode ~default:Rule.Mode.Standard
  end

  type t =
    { targets : String_with_vars.t Targets.t
    ; deps : Dep_conf.t Bindings.t
    ; action : Loc.t * Action_dune_lang.t
    ; mode : Rule.Mode.t
    ; locks : String_with_vars.t list
    ; loc : Loc.t
    ; enabled_if : Blang.t
    ; alias : Alias.Name.t option
    ; package : Package.t option
    }

  type action_or_field =
    | Action
    | Field

  let atom_table =
    String.Map.of_list_exn
      [ ("run", Action)
      ; ("chdir", Action)
      ; ("setenv", Action)
      ; ("with-stdout-to", Action)
      ; ("with-stderr-to", Action)
      ; ("with-outputs-to", Action)
      ; ("with-stdin-from", Action)
      ; ("ignore-stdout", Action)
      ; ("ignore-stderr", Action)
      ; ("ignore-outputs", Action)
      ; ("progn", Action)
      ; ("echo", Action)
      ; ("cat", Action)
      ; ("copy", Action)
      ; ("copy#", Action)
      ; ("copy-and-add-line-directive", Action)
      ; ("system", Action)
      ; ("bash", Action)
      ; ("write-file", Action)
      ; ("diff", Action)
      ; ("diff?", Action)
      ; ("targets", Field)
      ; ("target", Field)
      ; ("deps", Field)
      ; ("action", Field)
      ; ("locks", Field)
      ; ("fallback", Field)
      ; ("mode", Field)
      ; ("alias", Field)
      ; ("enabled_if", Field)
      ]

  let short_form =
    let+ loc, action = located Action_dune_lang.decode in
    { targets = Infer
    ; deps = Bindings.empty
    ; action = (loc, action)
    ; mode = Standard
    ; locks = []
    ; loc
    ; enabled_if = Blang.true_
    ; alias = None
    ; package = None
    }

  let long_form =
    let* deps =
      field "deps" (Bindings.decode Dep_conf.decode) ~default:Bindings.empty
    in
    String_with_vars.add_user_vars_to_decoding_env (Bindings.var_names deps)
      (let+ loc = loc
       and+ action = field "action" (located Action_dune_lang.decode)
       and+ targets = Targets.field
       and+ locks = field "locks" (repeat String_with_vars.decode) ~default:[]
       and+ () =
         let+ fallback =
           field_b
             ~check:
               (Dune_lang.Syntax.renamed_in Stanza.syntax (1, 0)
                  ~to_:"(mode fallback)")
             "fallback"
         in
         (* The "fallback" field was only allowed in jbuild file, which we don't
            support anymore. So this cannot be [true]. We just keep the parser
            to provide a nice error message for people switching from jbuilder
            to dune. *)
         assert (not fallback)
       and+ mode = field "mode" Mode.decode ~default:Mode.Standard
       and+ enabled_if =
         Enabled_if.decode ~allowed_vars:Any ~since:(Some (1, 4)) ()
       and+ package =
         field_o "package"
           (Dune_lang.Syntax.since Stanza.syntax (2, 0)
           >>> Stanza_common.Pkg.decode)
       and+ alias =
         field_o "alias"
           (Dune_lang.Syntax.since Stanza.syntax (2, 0) >>> Alias.Name.decode)
       in
       { targets; deps; action; mode; locks; loc; enabled_if; alias; package })

  let decode =
    peek_exn >>= function
    | List (_, Atom (loc, A s) :: _) -> (
      match String.Map.find atom_table s with
      | None ->
        User_error.raise ~loc
          [ Pp.text "Unknown action or rule field." ]
          ~hints:
            (User_message.did_you_mean s
               ~candidates:(String.Map.keys atom_table))
      | Some Field -> fields long_form
      | Some Action -> short_form)
    | sexp ->
      User_error.raise ~loc:(Dune_lang.Ast.loc sexp)
        [ Pp.textf "S-expression of the form (<atom> ...) expected" ]

  type lex_or_yacc =
    { modules : string list
    ; mode : Rule.Mode.t
    ; enabled_if : Blang.t
    }

  let ocamllex =
    (let+ modules = repeat string in
     { modules; mode = Standard; enabled_if = Blang.true_ })
    <|> fields
          (let+ modules = field "modules" (repeat string)
           and+ mode = Mode.field
           and+ enabled_if =
             Enabled_if.decode ~allowed_vars:Any ~since:(Some (1, 4)) ()
           in
           { modules; mode; enabled_if })

  let ocamlyacc = ocamllex

  let ocamllex_to_rule loc { modules; mode; enabled_if } =
    let module S = String_with_vars in
    List.map modules ~f:(fun name ->
        let src = name ^ ".mll" in
        let dst = name ^ ".ml" in
        { targets =
            (* CR-someday aalekseyev: want to use [multiplicity = One] here, but
               can't because this is might get parsed with old dune syntax where
               [multiplicity = One] is not supported. *)
            Static
              { targets = [ S.make_text loc dst ]; multiplicity = Multiple }
        ; deps = Bindings.singleton (Dep_conf.File (S.virt_text __POS__ src))
        ; action =
            ( loc
            , Chdir
                ( S.virt_pform __POS__ (Var Workspace_root)
                , Run
                    ( S.virt_text __POS__ "ocamllex"
                    , [ S.virt_text __POS__ "-q"
                      ; S.virt_text __POS__ "-o"
                      ; S.virt_pform __POS__ (Var Targets)
                      ; S.virt_pform __POS__ (Var Deps)
                      ] ) ) )
        ; mode
        ; locks = []
        ; loc
        ; enabled_if
        ; alias = None
        ; package = None
        })

  let ocamlyacc_to_rule loc { modules; mode; enabled_if } =
    let module S = String_with_vars in
    List.map modules ~f:(fun name ->
        let src = name ^ ".mly" in
        { targets =
            Static
              { targets =
                  List.map ~f:(S.make_text loc) [ name ^ ".ml"; name ^ ".mli" ]
              ; multiplicity = Multiple
              }
        ; deps = Bindings.singleton (Dep_conf.File (S.virt_text __POS__ src))
        ; action =
            ( loc
            , Chdir
                ( S.virt_pform __POS__ (Var Workspace_root)
                , Run
                    ( S.virt_text __POS__ "ocamlyacc"
                    , [ S.virt_pform __POS__ (Var Deps) ] ) ) )
        ; mode
        ; locks = []
        ; loc
        ; enabled_if
        ; alias = None
        ; package = None
        })
end

module Menhir = struct
  type t =
    { merge_into : string option
    ; flags : Ordered_set_lang.Unexpanded.t
    ; modules : string list
    ; mode : Rule.Mode.t
    ; loc : Loc.t
    ; infer : bool
    ; enabled_if : Blang.t
    }

  let decode =
    fields
      (let+ merge_into = field_o "merge_into" string
       and+ flags = Ordered_set_lang.Unexpanded.field "flags"
       and+ modules = field "modules" (repeat string)
       and+ mode = Rule.Mode.field
       and+ infer =
         field_o_b "infer"
           ~check:(Dune_lang.Syntax.since Menhir_stanza.syntax (2, 0))
       and+ menhir_syntax = Dune_lang.Syntax.get_exn Menhir_stanza.syntax
       and+ enabled_if =
         Enabled_if.decode ~allowed_vars:Any ~since:(Some (1, 4)) ()
       and+ loc = loc in
       let infer =
         match infer with
         | Some infer -> infer
         | None -> menhir_syntax >= (2, 0)
       in
       { merge_into; flags; modules; mode; loc; infer; enabled_if })

  type Stanza.t += T of t

  let () =
    Dune_project.Extension.register_simple Menhir_stanza.syntax
      (return [ ("menhir", decode >>| fun x -> [ T x ]) ])
end

module Alias_conf = struct
  type t =
    { name : Alias.Name.t
    ; deps : Dep_conf.t Bindings.t
    ; action : (Loc.t * Action_dune_lang.t) option
    ; locks : String_with_vars.t list
    ; package : Package.t option
    ; enabled_if : Blang.t
    ; loc : Loc.t
    }

  let decode =
    fields
      (let* deps =
         field "deps" (Bindings.decode Dep_conf.decode) ~default:Bindings.empty
       in
       String_with_vars.add_user_vars_to_decoding_env (Bindings.var_names deps)
         (let+ name = field "name" Alias.Name.decode
          and+ package = field_o "package" Stanza_common.Pkg.decode
          and+ action =
            field_o "action"
              (let extra_info =
                 "Use a rule stanza with the alias field instead"
               in
               let* () =
                 Dune_lang.Syntax.deleted_in ~extra_info Stanza.syntax (2, 0)
               in
               located Action_dune_lang.decode)
          and+ loc = loc
          and+ locks =
            field "locks" (repeat String_with_vars.decode) ~default:[]
          and+ enabled_if =
            field "enabled_if" Blang.decode ~default:Blang.true_
          in
          { name; deps; action; package; locks; enabled_if; loc }))
end

module Tests = struct
  type t =
    { exes : Executables.t
    ; locks : String_with_vars.t list
    ; package : Package.t option
    ; deps : Dep_conf.t Bindings.t
    ; enabled_if : Blang.t
    ; action : Action_dune_lang.t option
    }

  let gen_parse names =
    fields
      (let* deps =
         field "deps" (Bindings.decode Dep_conf.decode) ~default:Bindings.empty
       in
       String_with_vars.add_user_vars_to_decoding_env (Bindings.var_names deps)
         (let+ buildable = Buildable.decode Executable
          and+ link_flags = Ordered_set_lang.Unexpanded.field "link_flags"
          and+ names = names
          and+ package = field_o "package" Stanza_common.Pkg.decode
          and+ locks =
            field "locks" (repeat String_with_vars.decode) ~default:[]
          and+ modes =
            field "modes" Executables.Link_mode.Map.decode
              ~default:Executables.Link_mode.Map.default_for_tests
          and+ enabled_if =
            Enabled_if.decode ~allowed_vars:Any ~since:(Some (1, 4)) ()
          and+ action =
            field_o "action"
              (Dune_lang.Syntax.since ~fatal:false Stanza.syntax (1, 2)
              >>> Action_dune_lang.decode)
          and+ forbidden_libraries =
            field "forbidden_libraries"
              (Dune_lang.Syntax.since Stanza.syntax (2, 0)
              >>> repeat (located Lib_name.decode))
              ~default:[]
          in
          { exes =
              { Executables.link_flags
              ; link_deps = []
              ; modes
              ; optional = false
              ; buildable
              ; names
              ; package = None
              ; promote = None
              ; install_conf = None
              ; embed_in_plugin_libraries = []
              ; forbidden_libraries
              ; bootstrap_info = None
              ; enabled_if
              }
          ; locks
          ; package
          ; deps
          ; enabled_if
          ; action
          }))

  let multi = gen_parse (field "names" (repeat1 (located string)))

  let single = gen_parse (field "name" (located string) >>| List.singleton)
end

module Toplevel = struct
  type t =
    { name : string
    ; libraries : (Loc.t * Lib_name.t) list
    ; loc : Loc.t
    ; pps : Preprocess.Without_instrumentation.t Preprocess.t
    }

  let decode =
    let open Dune_lang.Decoder in
    fields
      (let+ loc = loc
       and+ name = field "name" string
       and+ libraries =
         field "libraries" (repeat (located Lib_name.decode)) ~default:[]
       and+ pps =
         field "preprocess"
           (Dune_lang.Syntax.since Stanza.syntax (2, 5) >>> Preprocess.decode)
           ~default:Preprocess.No_preprocessing
       in
       match pps with
       | Preprocess.Pps _
       | No_preprocessing ->
         { name; libraries; loc; pps }
       | Action (loc, _)
       | Future_syntax loc ->
         User_error.raise ~loc
           [ Pp.text
               "Toplevel does not currently support action or future_syntax \
                preprocessing."
           ])
end

module Copy_files = struct
  type t =
    { add_line_directive : bool
    ; alias : Alias.Name.t option
    ; mode : Rule.Mode.t
    ; enabled_if : Blang.t
    ; files : String_with_vars.t
    ; syntax_version : Dune_lang.Syntax.Version.t
    }

  let long_form =
    let check = Dune_lang.Syntax.since Stanza.syntax (2, 7) in
    let+ alias = field_o "alias" (check >>> Alias.Name.decode)
    and+ mode =
      field "mode" ~default:Rule.Mode.Standard (check >>> Rule.Mode.decode)
    and+ enabled_if =
      Enabled_if.decode ~allowed_vars:Any ~since:(Some (2, 8)) ()
    and+ files = field "files" (check >>> String_with_vars.decode)
    and+ syntax_version = Dune_lang.Syntax.get_exn Stanza.syntax in
    { add_line_directive = false
    ; alias
    ; mode
    ; enabled_if
    ; files
    ; syntax_version
    }

  let decode =
    peek_exn >>= function
    | List _ -> fields long_form
    | _ ->
      let+ files = String_with_vars.decode
      and+ syntax_version = Dune_lang.Syntax.get_exn Stanza.syntax in
      { add_line_directive = false
      ; alias = None
      ; mode = Standard
      ; enabled_if = Blang.true_
      ; files
      ; syntax_version
      }
end

module Documentation = struct
  type t =
    { loc : Loc.t
    ; package : Package.t
    ; mld_files : Ordered_set_lang.t
    }

  let decode =
    fields
      (let+ package = Stanza_common.Pkg.field ~stanza:"documentation"
       and+ mld_files = Ordered_set_lang.field "mld_files"
       and+ loc = loc in
       { loc; package; mld_files })
end

module Include_subdirs = struct
  type qualification =
    | Unqualified
    | Qualified

  type t =
    | No
    | Include of qualification

  let decode ~enable_qualified =
    let opts_list =
      [ ("no", No); ("unqualified", Include Unqualified) ]
      @
      if enable_qualified then
        [ ("qualified", Include Qualified) ]
      else
        []
    in
    enum opts_list
end

module Library_redirect = struct
  type 'old_name t =
    { project : Dune_project.t
    ; loc : Loc.t
    ; old_name : 'old_name
    ; new_public_name : Loc.t * Lib_name.t
    }

  module Local = struct
    type nonrec t = (Loc.t * Lib_name.Local.t) t

    let for_lib (lib : Library.t) ~new_public_name ~loc : t =
      { loc; new_public_name; old_name = lib.name; project = lib.project }

    let of_private_lib (lib : Library.t) : t option =
      match lib.visibility with
      | Public _
      | Private None ->
        None
      | Private (Some package) ->
        let loc, name = lib.name in
        let package_name = Package.name package in
        let new_public_name = (loc, Lib_name.mangled package_name name) in
        Some (for_lib lib ~loc ~new_public_name)

    let of_lib (lib : Library.t) : t option =
      let open Option.O in
      let* public_name =
        match lib.visibility with
        | Public plib -> Some plib.name
        | Private _ -> None
      in
      if Lib_name.equal (Lib_name.of_local lib.name) (snd public_name) then
        None
      else
        let loc = fst public_name in
        Some (for_lib lib ~loc ~new_public_name:public_name)
  end
end

module Deprecated_library_name = struct
  module Old_name = struct
    type deprecation =
      | Not_deprecated
      | Deprecated of { deprecated_package : Package.Name.t }

    type t = Public_lib.t * deprecation

    let decode =
      let+ public = Public_lib.decode ~allow_deprecated_names:true in
      let deprecation =
        let deprecated_package =
          Lib_name.package_name (Public_lib.name public)
        in
        if
          let name = Package.name (Public_lib.package public) in
          Package.Name.equal deprecated_package name
        then
          Not_deprecated
        else
          Deprecated { deprecated_package }
      in
      (public, deprecation)
  end

  type t = Old_name.t Library_redirect.t

  let old_public_name (t : t) = Public_lib.name (fst t.old_name)

  let decode =
    fields
      (let+ loc = loc
       and+ project = Dune_project.get_exn ()
       and+ old_name = field "old_public_name" Old_name.decode
       and+ new_public_name =
         field "new_public_name" (located Lib_name.decode)
       in
       let () =
         let loc, old_name = (fst old_name).name in
         if Lib_name.equal (snd new_public_name) old_name then
           User_error.raise ~loc
             [ Pp.text
                 "old_public_name cannot be the same as the new_public_name"
             ]
       in
       { Library_redirect.loc; project; old_name; new_public_name })
end

module Generate_sites_module = struct
  type t =
    { loc : Loc.t
    ; module_ : Module_name.t
    ; sourceroot : bool
    ; relocatable : bool
    ; sites : (Loc.t * Package.Name.t) list
    ; plugins : (Loc.t * (Package.Name.t * (Loc.t * Section.Site.t))) list
    }

  let decode =
    fields
      (let+ loc = loc
       and+ module_ = field "module" Module_name.decode
       and+ sourceroot = field_b "sourceroot"
       and+ relocatable = field_b "relocatable"
       and+ sites =
         field "sites" ~default:[] (repeat (located Package.Name.decode))
       and+ plugins =
         field "plugins" ~default:[]
           (repeat
              (located (pair Package.Name.decode (located Section.Site.decode))))
       in
       { loc; module_; sourceroot; relocatable; sites; plugins })
end

type Stanza.t +=
  | Library of Library.t
  | Foreign_library of Foreign.Library.t
  | Executables of Executables.t
  | Rule of Rule.t
  | Install of Install_conf.t
  | Alias of Alias_conf.t
  | Copy_files of Copy_files.t
  | Documentation of Documentation.t
  | Tests of Tests.t
  | Include_subdirs of Loc.t * Include_subdirs.t
  | Toplevel of Toplevel.t
  | Library_redirect of Library_redirect.Local.t
  | Deprecated_library_name of Deprecated_library_name.t
  | Cram of Cram_stanza.t
  | Generate_sites_module of Generate_sites_module.t
  | Plugin of Plugin.t

module Stanzas = struct
  type t = Stanza.t list

  type syntax =
    | OCaml
    | Plain

  let rules l = List.map l ~f:(fun x -> Rule x)

  let execs exe = [ Executables exe ]

  type Stanza.t += Include of Loc.t * string

  type constructors = (string * Stanza.t list Dune_lang.Decoder.t) list

  let stanzas : constructors =
    [ ( "library"
      , let+ x = Library.decode in
        let base = [ Library x ] in
        match Library_redirect.Local.of_lib x with
        | None -> base
        | Some r -> Library_redirect r :: base )
    ; ( "foreign_library"
      , let+ () = Dune_lang.Syntax.since Stanza.syntax (2, 0)
        and+ x = Foreign.Library.decode in
        [ Foreign_library x ] )
    ; ("executable", Executables.single >>| execs)
    ; ("executables", Executables.multi >>| execs)
    ; ( "rule"
      , let+ loc = loc
        and+ x = Rule.decode in
        [ Rule { x with loc } ] )
    ; ( "ocamllex"
      , let+ loc = loc
        and+ x = Rule.ocamllex in
        rules (Rule.ocamllex_to_rule loc x) )
    ; ( "ocamlyacc"
      , let+ loc = loc
        and+ x = Rule.ocamlyacc in
        rules (Rule.ocamlyacc_to_rule loc x) )
    ; ( "install"
      , let+ x = Install_conf.decode in
        [ Install x ] )
    ; ( "alias"
      , let+ x = Alias_conf.decode in
        [ Alias x ] )
    ; ( "copy_files"
      , let+ x = Copy_files.decode in
        [ Copy_files x ] )
    ; ( "copy_files#"
      , let+ x = Copy_files.decode in
        [ Copy_files { x with add_line_directive = true } ] )
    ; ( "include"
      , let+ loc = loc
        and+ fn = relative_file in
        [ Include (loc, fn) ] )
    ; ( "documentation"
      , let+ d = Documentation.decode in
        [ Documentation d ] )
    ; ( "jbuild_version"
      , let+ () = Dune_lang.Syntax.deleted_in Stanza.syntax (1, 0)
        and+ _ = Jbuild_version.decode in
        [] )
    ; ( "tests"
      , let+ () = Dune_lang.Syntax.since Stanza.syntax (1, 0)
        and+ t = Tests.multi in
        [ Tests t ] )
    ; ( "test"
      , let+ () = Dune_lang.Syntax.since Stanza.syntax (1, 0)
        and+ t = Tests.single in
        [ Tests t ] )
    ; ( "external_variant"
      , let+ () = Dune_lang.Syntax.deleted_in Stanza.syntax (2, 6) in
        [] )
    ; ( "env"
      , let+ x = Dune_env.Stanza.decode in
        [ Dune_env.T x ] )
    ; ( "include_subdirs"
      , let* project = Dune_project.get_exn () in
        let+ () = Dune_lang.Syntax.since Stanza.syntax (1, 1)
        and+ t =
          let enable_qualified =
            Option.is_some
              (Dune_project.find_extension_args project Coq_stanza.key)
          in
          Include_subdirs.decode ~enable_qualified
        and+ loc = loc in
        [ Include_subdirs (loc, t) ] )
    ; ( "toplevel"
      , let+ () = Dune_lang.Syntax.since Stanza.syntax (1, 7)
        and+ t = Toplevel.decode in
        [ Toplevel t ] )
    ; ( "deprecated_library_name"
      , let+ () = Dune_lang.Syntax.since Stanza.syntax (2, 0)
        and+ t = Deprecated_library_name.decode in
        [ Deprecated_library_name t ] )
    ; ( "cram"
      , let+ () = Dune_lang.Syntax.since Stanza.syntax (2, 7)
        and+ t = Cram_stanza.decode
        and+ project = Dune_project.get_exn ()
        and+ loc = loc in
        if not (Dune_project.cram project) then
          User_warning.emit ~loc
            ~is_error:(Dune_project.dune_version project >= (3, 0))
            [ Pp.text "Cram tests are not enabled in this project." ]
            ~hints:
              [ Pp.text
                  "You can enable cram tests by adding (cram enable) to your \
                   dune-project file."
              ];
        [ Cram t ] )
    ; ( "generate_sites_module"
      , let+ () = Dune_lang.Syntax.since Section.dune_site_syntax (0, 1)
        and+ t = Generate_sites_module.decode in
        [ Generate_sites_module t ] )
    ; ( "plugin"
      , let+ () = Dune_lang.Syntax.since Section.dune_site_syntax (0, 1)
        and+ t = Plugin.decode in
        [ Plugin t ] )
    ]

  let () = Dune_project.Lang.register Stanza.syntax stanzas

  let parser project =
    let syntax_parser = Dune_project.stanza_parser project in
    Dune_project.set project syntax_parser

  let parse parser = Dune_lang.Decoder.parse parser Univ_map.empty

  let of_ast (project : Dune_project.t) sexp =
    let parser = parser project in
    parse parser sexp

  let rec parse_file_includes ~stanza_parser ~context sexps =
    List.concat_map sexps ~f:(parse stanza_parser)
    |> List.concat_map ~f:(function
         | Include (loc, fn) ->
           let sexps, context = Include_stanza.load_sexps ~context (loc, fn) in
           parse_file_includes ~stanza_parser ~context sexps
         | stanza -> [ stanza ])

  let parse ~file (project : Dune_project.t) sexps =
    let stanza_parser = parser project in
    let stanzas =
      let context = Include_stanza.in_file file in
      parse_file_includes ~stanza_parser ~context sexps
    in
    let (_ : bool) =
      List.fold_left stanzas ~init:false ~f:(fun env stanza ->
          match stanza with
          | Dune_env.T e ->
            if env then
              User_error.raise ~loc:e.loc
                [ Pp.text "The 'env' stanza cannot appear more than once" ]
            else
              true
          | _ -> env)
    in
    stanzas
end

let stanza_package = function
  | Library lib -> Library.package lib
  | Alias { package = Some package; _ }
  | Rule { package = Some package; _ }
  | Install { package; _ }
  | Plugin { package; _ }
  | Executables { install_conf = Some { package; _ }; _ }
  | Documentation { package; _ }
  | Tests { package = Some package; _ } ->
    Some package
  | Coq_stanza.Theory.T { package = Some package; _ } -> Some package
  | _ -> None

type t =
  { dir : Path.Source.t
  ; project : Dune_project.t
  ; stanzas : Stanzas.t
  }

let parse sexps ~dir ~file ~project =
  let stanzas = Stanzas.parse ~file project sexps in
  let stanzas =
    if !Clflags.ignore_promoted_rules then
      List.filter stanzas ~f:(function
        | Rule { mode = Rule.Mode.Promote { only = None; _ }; _ }
        | Menhir.T { mode = Rule.Mode.Promote { only = None; _ }; _ } ->
          false
        | _ -> true)
    else
      stanzas
  in
  { dir; project; stanzas }

let rec fold_stanzas l ~init ~f =
  match l with
  | [] -> init
  | t :: l -> inner_fold t t.stanzas l ~init ~f

and inner_fold t inner_list l ~init ~f =
  match inner_list with
  | [] -> fold_stanzas l ~init ~f
  | x :: inner_list -> inner_fold t inner_list l ~init:(f t x init) ~f
