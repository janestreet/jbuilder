open! Dune_engine
open! Stdune

module Vfile = Dune_lang.Versioned_file.Make (struct
  type t = unit
end)

let fn = "dune-package"

module Lib = struct
  type t =
    { info : Path.t Lib_info.t
    ; modules : Modules.t option
    ; main_module_name : Module_name.t option
    }

  let make ~info ~main_module_name ~modules =
    let obj_dir = Lib_info.obj_dir info in
    let dir = Obj_dir.dir obj_dir in
    let map_path p =
      if Path.is_managed p then
        Path.relative dir (Path.basename p)
      else
        p
    in
    let info = Lib_info.map_path info ~f:map_path in
    { info; main_module_name; modules }

  let of_dune_lib ~info ~main_module_name ~modules =
    make ~info ~main_module_name ~modules:(Some modules)

  let of_findlib info = make ~info ~main_module_name:None ~modules:None

  let dir_of_name name =
    let _, components = Lib_name.split name in
    Path.Local.L.relative Path.Local.root components

  let encode ~package_root { info; main_module_name; modules } =
    let open Dune_lang.Encoder in
    let no_loc f (_loc, x) = f x in
    let path = Dpath.Local.encode ~dir:package_root in
    let paths name f = field_l name path f in
    let mode_paths name (xs : Path.t Mode.Dict.List.t) =
      field_l name sexp (Mode.Dict.List.encode path xs)
    in
    let libs name = field_l name (no_loc Lib_name.encode) in
    let name = Lib_info.name info in
    let kind = Lib_info.kind info in
    let modes = Lib_info.modes info in
    let synopsis = Lib_info.synopsis info in
    let obj_dir = Lib_info.obj_dir info in
    let orig_src_dir = Lib_info.orig_src_dir info in
    let implements = Lib_info.implements info in
    let ppx_runtime_deps = Lib_info.ppx_runtime_deps info in
    let default_implementation = Lib_info.default_implementation info in
    let special_builtin_support = Lib_info.special_builtin_support info in
    let archives = Lib_info.archives info in
    let sub_systems = Lib_info.sub_systems info in
    let plugins = Lib_info.plugins info in
    let requires = Lib_info.requires info in
    let foreign_objects =
      match Lib_info.foreign_objects info with
      | External e -> e
      | Local -> assert false
    in
    let jsoo_runtime = Lib_info.jsoo_runtime info in
    let virtual_ = Option.is_some (Lib_info.virtual_ info) in
    let instrumentation_backend = Lib_info.instrumentation_backend info in
    let native_archives =
      match Lib_info.native_archives info with
      | Lib_info.Files f -> f
      | Needs_module_info _ ->
        Code_error.raise "caller must set native archives to known value" []
    in
    record_fields
    @@ [ field "name" Lib_name.encode name
       ; field "kind" Lib_kind.encode kind
       ; field_b "virtual" virtual_
       ; field_o "synopsis" string synopsis
       ; field_o "orig_src_dir" path orig_src_dir
       ; mode_paths "archives" archives
       ; mode_paths "plugins" plugins
       ; paths "foreign_objects" foreign_objects
       ; paths "foreign_archives" (Lib_info.foreign_archives info)
       ; paths "native_archives" native_archives
       ; paths "jsoo_runtime" jsoo_runtime
       ; Lib_dep.L.field_encode requires ~name:"requires"
       ; libs "ppx_runtime_deps" ppx_runtime_deps
       ; field_o "implements" (no_loc Lib_name.encode) implements
       ; field_o "default_implementation" (no_loc Lib_name.encode)
           default_implementation
       ; field_o "main_module_name" Module_name.encode main_module_name
       ; field_l "modes" sexp (Mode.Dict.Set.encode modes)
       ; field_l "obj_dir" sexp (Obj_dir.encode obj_dir)
       ; field_o "modules" Modules.encode modules
       ; field_o "special_builtin_support"
           Lib_info.Special_builtin_support.encode special_builtin_support
       ; field_o "instrumentation.backend" (no_loc Lib_name.encode)
           instrumentation_backend
       ]
    @ Sub_system_name.Map.to_list_map sub_systems ~f:(fun name info ->
          let (module S) = Sub_system_info.get name in
          match info with
          | S.T info ->
            let _ver, sexps = S.encode info in
            field_l (Sub_system_name.to_string name) sexp sexps
          | _ -> assert false)

  let decode ~(lang : Vfile.Lang.Instance.t) ~base =
    let open Dune_lang.Decoder in
    let path = Dpath.Local.decode ~dir:base in
    let field_l s x = field ~default:[] s (repeat x) in
    let libs s = field_l s (located Lib_name.decode) in
    let paths s = field_l s path in
    let mode_paths name =
      field ~default:Mode.Dict.List.empty name (Mode.Dict.List.decode path)
    in
    fields
      (let* main_module_name = field_o "main_module_name" Module_name.decode in
       let* implements = field_o "implements" (located Lib_name.decode) in
       let* default_implementation =
         field_o "default_implementation" (located Lib_name.decode)
       in
       let* name = field "name" Lib_name.decode in
       let dir = Path.append_local base (dir_of_name name) in
       let* obj_dir = field_o "obj_dir" (Obj_dir.decode ~dir) in
       let obj_dir =
         match obj_dir with
         | None -> Obj_dir.make_external_no_private ~dir
         | Some obj_dir -> obj_dir
       in
       let+ synopsis = field_o "synopsis" string
       and+ loc = loc
       and+ modes = field_l "modes" Mode.decode
       and+ kind = field "kind" Lib_kind.decode
       and+ archives = mode_paths "archives"
       and+ plugins = mode_paths "plugins"
       and+ foreign_objects = paths "foreign_objects"
       and+ foreign_archives =
         if lang.version >= (2, 0) then
           paths "foreign_archives"
         else
           let+ m = mode_paths "foreign_archives" in
           m.byte
       and+ native_archives = paths "native_archives"
       and+ jsoo_runtime = paths "jsoo_runtime"
       and+ requires = field_l "requires" (Lib_dep.decode ~allow_re_export:true)
       and+ ppx_runtime_deps = libs "ppx_runtime_deps"
       and+ virtual_ = field_b "virtual"
       and+ sub_systems = Sub_system_info.record_parser ()
       and+ orig_src_dir = field_o "orig_src_dir" path
       and+ modules =
         let src_dir = Obj_dir.dir obj_dir in
         field "modules"
           (Modules.decode
              ~implements:(Option.is_some implements)
              ~src_dir ~version:lang.version)
       and+ special_builtin_support =
         field_o "special_builtin_support"
           (Dune_lang.Syntax.since Stanza.syntax (1, 10)
           >>> Lib_info.Special_builtin_support.decode)
       and+ instrumentation_backend =
         field_o "instrumentation.backend" (located Lib_name.decode)
       in
       let modes = Mode.Dict.Set.of_list modes in
       let entry_modules =
         Modules.entry_modules modules |> List.map ~f:Module.name
       in
       let info : Path.t Lib_info.t =
         let src_dir = Obj_dir.dir obj_dir in
         let enabled = Lib_info.Enabled_status.Normal in
         let status =
           match Lib_name.analyze name with
           | Private (_, _) -> Lib_info.Status.Installed_private
           | Public (_, _) -> Lib_info.Status.Installed
         in
         let version = None in
         let main_module_name = Lib_info.Inherited.This main_module_name in
         let foreign_objects = Lib_info.Source.External foreign_objects in
         let jsoo_archive = None in
         let preprocess = Preprocess.Per_module.no_preprocessing () in
         let virtual_deps = [] in
         let dune_version = None in
         let virtual_ =
           if virtual_ then
             Some (Lib_info.Source.External modules)
           else
             None
         in
         let wrapped =
           Some (Lib_info.Inherited.This (Modules.wrapped modules))
         in
         let entry_modules = Lib_info.Source.External (Ok entry_modules) in
         Lib_info.create ~path_kind:External ~loc ~name ~kind ~status ~src_dir
           ~orig_src_dir ~obj_dir ~version ~synopsis ~main_module_name
           ~sub_systems ~requires ~foreign_objects ~plugins ~archives
           ~ppx_runtime_deps ~foreign_archives
           ~native_archives:(Files native_archives) ~foreign_dll_files:[]
           ~jsoo_runtime ~jsoo_archive ~preprocess ~enabled ~virtual_deps
           ~dune_version ~virtual_ ~entry_modules ~implements
           ~default_implementation ~modes ~wrapped ~special_builtin_support
           ~exit_module:None ~instrumentation_backend
       in
       { info; main_module_name; modules = Some modules })

  let modules t = t.modules

  let main_module_name t = t.main_module_name

  let wrapped t = Option.map t.modules ~f:Modules.wrapped

  let info dp = dp.info

  let to_dyn { info; modules; main_module_name } =
    let open Dyn.Encoder in
    record
      [ ("info", Lib_info.to_dyn Path.to_dyn info)
      ; ("modules", option Modules.to_dyn modules)
      ; ("main_module_name", option Module_name.to_dyn main_module_name)
      ]
end

module Deprecated_library_name = struct
  type t =
    { loc : Loc.t
    ; old_public_name : Lib_name.t
    ; new_public_name : Lib_name.t
    }

  let decode =
    let open Dune_lang.Decoder in
    Dune_lang.Syntax.since Stanza.syntax (2, 0)
    >>> fields
          (let+ old_public_name = field "old_public_name" Lib_name.decode
           and+ new_public_name = field "new_public_name" Lib_name.decode
           and+ loc = loc in
           { loc; old_public_name; new_public_name })

  let encode { loc = _; old_public_name; new_public_name } =
    let open Dune_lang.Encoder in
    record_fields
      [ field "old_public_name" Lib_name.encode old_public_name
      ; field "new_public_name" Lib_name.encode new_public_name
      ]

  let to_dyn { loc = _; old_public_name; new_public_name } =
    let open Dyn.Encoder in
    record
      [ ("old_public_name", Lib_name.to_dyn old_public_name)
      ; ("new_public_name", Lib_name.to_dyn new_public_name)
      ]
end

module Entry = struct
  type t =
    | Library of Lib.t
    | Deprecated_library_name of Deprecated_library_name.t
    | Hidden_library of Lib.t

  let name = function
    | Library lib
    | Hidden_library lib ->
      Lib_info.name (Lib.info lib)
    | Deprecated_library_name d -> d.old_public_name

  let version = function
    | Library lib
    | Hidden_library lib ->
      Lib_info.version (Lib.info lib)
    | Deprecated_library_name _ -> None

  let loc = function
    | Library lib
    | Hidden_library lib ->
      Lib_info.loc (Lib.info lib)
    | Deprecated_library_name d -> d.loc

  let cstrs ~lang ~dir =
    let open Dune_lang.Decoder in
    [ ( "library"
      , let+ lib = Lib.decode ~lang ~base:dir in
        Library lib )
    ; ( "deprecated_library_name"
      , let+ x = Deprecated_library_name.decode in
        Deprecated_library_name x )
    ]

  let to_dyn x =
    let open Dyn.Encoder in
    match x with
    | Library lib -> constr "Library" [ Lib.to_dyn lib ]
    | Deprecated_library_name lib ->
      constr "Deprecated_library_name" [ Deprecated_library_name.to_dyn lib ]
    | Hidden_library lib -> constr "Hidden_library" [ Lib.to_dyn lib ]
end

type t =
  { name : Package.Name.t
  ; entries : Entry.t Lib_name.Map.t
  ; version : string option
  ; sections : Path.t Section.Map.t
  ; sites : Section.t Section.Site.Map.t
  ; dir : Path.t
  ; files : (Section.t * Install.Dst.t list) list
  }

let decode ~lang ~dir =
  let open Dune_lang.Decoder in
  let+ name = field "name" Package.Name.decode
  and+ version = field_o "version" string
  and+ sections =
    field ~default:[] "sections"
      (repeat (pair (located Section.decode) Dpath.External.decode))
  and+ sites =
    field ~default:[] "sites"
      (repeat (pair (located Section.Site.decode) Section.decode))
  and+ files =
    field ~default:[] "files"
      (repeat (pair Install.Section.decode (enter (repeat Install.Dst.decode))))
  and+ entries = leftover_fields_as_sums (Entry.cstrs ~lang ~dir) in
  let entries =
    List.map entries ~f:(fun e ->
        let e =
          match (e : Entry.t) with
          | Library lib ->
            let info = Lib_info.set_version lib.info version in
            Entry.Library { lib with info }
          | _ -> e
        in
        (Entry.name e, e))
    |> Lib_name.Map.of_list
    |> function
    | Ok x -> x
    | Error (name, _e1, e2) ->
      User_error.raise ~loc:(Entry.loc e2)
        [ Pp.textf "Library %s is defined for the second time."
            (Lib_name.to_string name)
        ]
  in
  let section_map of_list_map to_string sections =
    match of_list_map sections ~f:(fun ((_, k), v) -> (k, v)) with
    | Ok x -> x
    | Error (s, ((loc, _), _), _) ->
      User_error.raise ~loc
        [ Pp.textf "The section %s appears multiple times" (to_string s) ]
  in
  let sections =
    let sections = List.map sections ~f:(fun (k, v) -> (k, Path.external_ v)) in
    section_map Section.Map.of_list_map Section.to_string sections
  in
  let sites =
    section_map Section.Site.Map.of_list_map Section.Site.to_string sites
  in
  { name; version; entries; dir; sections; sites; files }

let () = Vfile.Lang.register Stanza.syntax ()

let prepend_version ~dune_version sexps =
  let open Dune_lang.Encoder in
  let list s = Dune_lang.List s in
  [ list
      [ Dune_lang.atom "lang"
      ; string (Dune_lang.Syntax.name Stanza.syntax)
      ; Dune_lang.Syntax.Version.encode dune_version
      ]
  ]
  @ sexps

let encode ~dune_version { entries; name; version; dir; sections; sites; files }
    =
  let open Dune_lang.Encoder in
  let sections =
    Section.Map.to_list_map sections ~f:(fun k v ->
        (k, Path.to_absolute_filename v))
  in
  let sites = Section.Site.Map.to_list sites in
  let sexp =
    record_fields
      [ field "name" Package.Name.encode name
      ; field_o "version" string version
      ; field_l "sections" (pair Section.encode string) sections
      ; field_l "sites" (pair Section.Site.encode Section.encode) sites
      ; field_l "files" (pair Section.encode (list Install.Dst.encode)) files
      ]
  in
  let list s = Dune_lang.List s in
  let entries =
    Lib_name.Map.to_list_map entries ~f:(fun _name e ->
        match e with
        | Entry.Library lib ->
          list (Dune_lang.atom "library" :: Lib.encode lib ~package_root:dir)
        | Deprecated_library_name d ->
          list
            (Dune_lang.atom "deprecated_library_name"
            :: Deprecated_library_name.encode d)
        | Hidden_library lib ->
          Code_error.raise "Dune_package.encode got Hidden_library"
            [ ("lib", Lib.to_dyn lib) ])
  in
  prepend_version ~dune_version (List.concat [ sexp; entries ])

let to_dyn { entries; name; version; dir; sections; sites; files } =
  let open Dyn.Encoder in
  record
    [ ("entries", list Entry.to_dyn (Lib_name.Map.values entries))
    ; ("name", Package.Name.to_dyn name)
    ; ("version", option string version)
    ; ("dir", Path.to_dyn dir)
    ; ("sections", Section.Map.to_dyn Path.to_dyn sections)
    ; ("sites", Section.Site.Map.to_dyn Section.to_dyn sites)
    ; ("files", (list (pair Section.to_dyn (list Install.Dst.to_dyn))) files)
    ]

module Or_meta = struct
  type nonrec t =
    | Use_meta
    | Dune_package of t

  let encode ~dune_version = function
    | Use_meta ->
      prepend_version ~dune_version [ Dune_lang.(List [ atom "use_meta" ]) ]
    | Dune_package p -> encode ~dune_version p

  let decode ~lang ~dir =
    let open Dune_lang.Decoder in
    fields
      (let* use_meta = field_b "use_meta" in
       if use_meta then
         return Use_meta
       else
         let+ package = decode ~lang ~dir in
         Dune_package package)

  let load p =
    let dir = Path.parent_exn p in
    Vfile.load p ~f:(fun lang ->
        String_with_vars.set_decoding_env
          (Pform.Env.initial lang.version)
          (decode ~lang ~dir))

  let pp ~dune_version ppf t =
    let t = encode ~dune_version t in
    Format.fprintf ppf "%a@."
      (Format.pp_print_list ~pp_sep:Format.pp_print_newline
         Dune_lang.Deprecated.pp)
      t

  let to_dyn x =
    let open Dyn.Encoder in
    match x with
    | Use_meta -> constr "Use_meta" []
    | Dune_package t -> constr "Dune_package" [ to_dyn t ]
end
