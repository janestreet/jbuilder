open! Stdune
open Import
open Dune_lang.Decoder

module Name : sig
  type t = private
    | Named of string
    | Anonymous of Path.Source.t

  val to_dyn : t -> Dyn.t

  val equal : t -> t -> bool

  val compare : t -> t -> Ordering.t

  val to_string_hum : t -> string

  val decode : t Dune_lang.Decoder.t

  val to_encoded_string : t -> string

  val of_encoded_string : string -> t

  val anonymous : Path.Source.t -> t

  val named : string -> t option

  module Infix : Comparator.OPS with type t = t

  module Map : Map.S with type key = t
end = struct
  module T = struct
    type t =
      | Named of string
      | Anonymous of Path.Source.t

    let compare a b =
      match (a, b) with
      | Named x, Named y -> String.compare x y
      | Anonymous x, Anonymous y -> Path.Source.compare x y
      | Named _, Anonymous _ -> Lt
      | Anonymous _, Named _ -> Gt

    let equal a b = Ordering.is_eq (compare a b)

    let to_dyn =
      let open Dyn.Encoder in
      function
      | Named n -> constr "Named" [ string n ]
      | Anonymous p -> constr "Anonymous" [ Path.Source.to_dyn p ]
  end

  include T
  module Map = Map.Make (T)
  module Infix = Comparator.Operators (T)

  let anonymous_root = Anonymous Path.Source.root

  let to_string_hum = function
    | Named s -> s
    | Anonymous p ->
      sprintf "<anonymous %s>" (Path.Source.to_string_maybe_quoted p)

  let validate name =
    let len = String.length name in
    len > 0
    && String.for_all name ~f:(function
         | '.'
         | '/' ->
           false
         | _ -> true)

  let named name =
    if validate name then
      Some (Named name)
    else
      None

  let anonymous path = Anonymous path

  let decode =
    Dune_lang.Decoder.plain_string (fun ~loc s ->
        if validate s then
          Named s
        else
          User_error.raise ~loc [ Pp.text "Invalid project name" ])

  let to_encoded_string = function
    | Named s -> s
    | Anonymous p ->
      if Path.Source.is_root p then
        "."
      else
        "."
        ^ String.map (Path.Source.to_string p) ~f:(function
            | '/' -> '.'
            | c -> c)

  let of_encoded_string =
    let invalid s =
      (* Users would see this error if they did "dune build
         _build/default/.ppx/..." *)
      User_error.raise [ Pp.textf "Invalid encoded project name: %S" s ]
    in
    fun s ->
      match s with
      | "" -> invalid s
      | "." -> anonymous_root
      | _ when s.[0] = '.' -> (
        match
          String.split s ~on:'.' |> List.tl |> String.concat ~sep:"/"
          |> Path.of_string |> Path.as_in_source_tree
        with
        | Some p -> Anonymous p
        | None -> invalid s)
      | _ when validate s -> Named s
      | _ -> invalid s
end

module File_key = struct
  type t = string

  module Map = String.Map

  let of_string s = s

  let to_string s = s

  let make ~name ~root =
    let digest = Digest.generic (name, root) |> Digest.to_string in
    String.take digest 12
end

type t =
  { name : Name.t
  ; root : Path.Source.t
  ; version : string option
  ; dune_version : Dune_lang.Syntax.Version.t
  ; info : Package.Info.t
  ; packages : Package.t Package.Name.Map.t
  ; stanza_parser : Stanza.t list Dune_lang.Decoder.t
  ; project_file : Path.Source.t
  ; extension_args : Univ_map.t
  ; parsing_context : Univ_map.t
  ; implicit_transitive_deps : bool
  ; wrapped_executables : bool
  ; executables_implicit_empty_intf : bool
  ; accept_alternative_dune_file_name : bool
  ; generate_opam_files : bool
  ; use_standard_c_and_cxx_flags : bool option
  ; file_key : File_key.t
  ; dialects : Dialect.DB.t
  ; explicit_js_mode : bool
  ; format_config : Format_config.t option
  ; strict_package_deps : bool
  ; cram : bool
  }

let equal = ( == )

let hash = Hashtbl.hash

let parsing_context t = t.parsing_context

let packages t = t.packages

let version t = t.version

let name t = t.name

let root t = t.root

let stanza_parser t = t.stanza_parser

let file t = t.project_file

let file_key t = t.file_key

let implicit_transitive_deps t = t.implicit_transitive_deps

let generate_opam_files t = t.generate_opam_files

let use_standard_c_and_cxx_flags t = t.use_standard_c_and_cxx_flags

let dialects t = t.dialects

let explicit_js_mode t = t.explicit_js_mode

let dune_version t = t.dune_version

let to_dyn
    { name
    ; root
    ; version
    ; dune_version
    ; info
    ; project_file
    ; parsing_context = _
    ; extension_args = _
    ; stanza_parser = _
    ; packages
    ; implicit_transitive_deps
    ; wrapped_executables
    ; executables_implicit_empty_intf
    ; accept_alternative_dune_file_name
    ; generate_opam_files
    ; use_standard_c_and_cxx_flags
    ; file_key
    ; dialects
    ; explicit_js_mode
    ; format_config
    ; strict_package_deps
    ; cram
    } =
  let open Dyn.Encoder in
  record
    [ ("name", Name.to_dyn name)
    ; ("root", Path.Source.to_dyn root)
    ; ("version", (option string) version)
    ; ("dune_version", Dune_lang.Syntax.Version.to_dyn dune_version)
    ; ("info", Package.Info.to_dyn info)
    ; ("project_file", Path.Source.to_dyn project_file)
    ; ( "packages"
      , (list (pair Package.Name.to_dyn Package.to_dyn))
          (Package.Name.Map.to_list packages) )
    ; ("implicit_transitive_deps", bool implicit_transitive_deps)
    ; ("wrapped_executables", bool wrapped_executables)
    ; ("executables_implicit_empty_intf", bool executables_implicit_empty_intf)
    ; ( "accept_alternative_dune_file_name"
      , bool accept_alternative_dune_file_name )
    ; ("generate_opam_files", bool generate_opam_files)
    ; ("use_standard_c_and_cxx_flags", option bool use_standard_c_and_cxx_flags)
    ; ("file_key", string file_key)
    ; ("dialects", Dialect.DB.to_dyn dialects)
    ; ("explicit_js_mode", bool explicit_js_mode)
    ; ("format_config", option Format_config.to_dyn format_config)
    ; ("strict_package_deps", bool strict_package_deps)
    ; ("cram", bool cram)
    ]

let find_extension_args t key = Univ_map.find t.extension_args key

include Dune_lang.Versioned_file.Make (struct
  type t = Stanza.Parser.t list
end)

let default_dune_language_version =
  ref (Dune_lang.Syntax.greatest_supported_version Stanza.syntax)

let get_dune_lang () =
  { (Lang.get_exn "dune") with version = !default_dune_language_version }

type created_or_already_exist =
  | Created
  | Already_exist

module Extension = struct
  type 'a t = 'a Univ_map.Key.t

  type 'a poly_info =
    { syntax : Dune_lang.Syntax.t
    ; stanzas : ('a * Stanza.Parser.t list) Dune_lang.Decoder.t
    ; key : 'a t
    }

  type packed_extension = Packed : 'a poly_info -> packed_extension

  type info =
    | Extension of packed_extension
    | Deleted_in of Dune_lang.Syntax.Version.t

  type instance =
    { extension : packed_extension
    ; version : Dune_lang.Syntax.Version.t
    ; loc : Loc.t
    ; parse_args :
           (Univ_map.t * Stanza.Parser.t list) Dune_lang.Decoder.t
        -> Univ_map.t * Stanza.Parser.t list
    }

  (* CR-someday amokhov: convert this mutable table to a memoized function,
     which depends on the contents of dune files that declare extensions. *)
  let extensions = Table.create (module String) 32

  let register syntax stanzas arg_to_dyn =
    let name = Dune_lang.Syntax.name syntax in
    if Table.mem extensions name then
      Code_error.raise "Dune_project.Extension.register: already registered"
        [ ("name", Dyn.Encoder.string name) ];
    let key = Univ_map.Key.create ~name arg_to_dyn in
    let ext = { syntax; stanzas; key } in
    Table.add_exn extensions name (Extension (Packed ext));
    key

  let register_deleted ~name ~deleted_in =
    Table.add_exn extensions name (Deleted_in deleted_in)

  let register_simple syntax stanzas =
    let unit_stanzas =
      let+ r = stanzas in
      ((), r)
    in
    let (_ : unit t) = register syntax unit_stanzas Unit.to_dyn in
    ()

  let instantiate ~dune_lang_ver ~loc ~parse_args (name_loc, name) (ver_loc, ver)
      =
    match Table.find extensions name with
    | None ->
      User_error.raise ~loc:name_loc
        [ Pp.textf "Unknown extension %S." name ]
        ~hints:
          (User_message.did_you_mean name ~candidates:(Table.keys extensions))
    | Some (Deleted_in v) ->
      User_error.raise ~loc
        [ Pp.textf
            "Extension %s was deleted in the %s version of the dune language"
            name
            (Dune_lang.Syntax.Version.to_string v)
        ]
    | Some (Extension (Packed e)) ->
      Dune_lang.Syntax.check_supported ~dune_lang_ver e.syntax (ver_loc, ver);
      { extension = Packed e; version = ver; loc; parse_args }

  type automatic =
    | Selected of instance
    | Not_selected of packed_extension

  let automatic ~explicitly_selected : automatic list =
    Table.foldi extensions ~init:[] ~f:(fun name extension acc ->
        match String.Map.find explicitly_selected name with
        | Some instance -> Selected instance :: acc
        | None -> (
          match extension with
          | Deleted_in _ -> acc
          | Extension e -> Not_selected e :: acc))
end

let interpret_lang_and_extensions ~(lang : Lang.Instance.t) ~explicit_extensions
    =
  match
    String.Map.of_list
      (List.map explicit_extensions ~f:(fun (e : Extension.instance) ->
           let syntax =
             let (Packed e) = e.extension in
             e.syntax
           in
           (Dune_lang.Syntax.name syntax, e)))
  with
  | Error (name, _, ext) ->
    User_error.raise ~loc:ext.loc
      [ Pp.textf "Extension %S specified for the second time." name ]
  | Ok map ->
    let extensions = Extension.automatic ~explicitly_selected:map in
    let parsing_context =
      let init =
        Univ_map.singleton
          (Dune_lang.Syntax.key lang.syntax)
          (Active lang.version)
      in
      let init =
        Univ_map.set init String_with_vars.decoding_env_key
          (Pform.Env.initial lang.version)
      in
      List.fold_left extensions ~init ~f:(fun acc (ext : Extension.automatic) ->
          let syntax =
            let (Extension.Packed ext) =
              match ext with
              | Selected e -> e.extension
              | Not_selected e -> e
            in
            ext.syntax
          in
          let status : Dune_lang.Syntax.Key.t =
            match ext with
            | Selected ext -> Active ext.version
            | Not_selected (Packed e) ->
              Inactive
                { lang = e.syntax; dune_lang_ver = lang.Lang.Instance.version }
          in
          Univ_map.set acc (Dune_lang.Syntax.key syntax) status)
    in
    let extension_args, extension_stanzas =
      List.fold_left extensions ~init:(Univ_map.empty, [])
        ~f:(fun (args_acc, stanzas_acc) (ext : Extension.automatic) ->
          match ext with
          | Not_selected (Packed e) ->
            let stanzas =
              let open Dune_lang.Decoder in
              let _arg, stanzas =
                let parsing_context =
                  (* Temporarily mark the extension as active so that we can
                     call the parser and extract the list of stanza names this
                     extension registers *)
                  Univ_map.set parsing_context
                    (Dune_lang.Syntax.key e.syntax)
                    (Active
                       (Dune_lang.Syntax.greatest_supported_version e.syntax))
                in
                parse (enter e.stanzas) parsing_context
                  (List (Loc.of_pos __POS__, []))
              in
              List.map stanzas ~f:(fun (name, _) ->
                  ( name
                  , let+ _ = Dune_lang.Syntax.get_exn e.syntax in
                    (* The above [get_exn] will raise because the extension is
                       inactive *)
                    assert false ))
            in
            (args_acc, stanzas :: stanzas_acc)
          | Selected instance ->
            let (Packed e) = instance.extension in
            let args =
              let+ arg, stanzas =
                Dune_lang.Decoder.set_many parsing_context e.stanzas
              in
              (Univ_map.set args_acc e.key arg, stanzas)
            in
            let args_acc, stanzas = instance.parse_args args in
            (args_acc, stanzas :: stanzas_acc))
    in
    let stanzas = List.concat (lang.data :: extension_stanzas) in
    let stanza_parser =
      Dune_lang.Decoder.(set_many parsing_context (sum stanzas))
    in
    (parsing_context, stanza_parser, extension_args)

let key = Univ_map.Key.create ~name:"dune-project" to_dyn

let set t = Dune_lang.Decoder.set key t

let get_exn () =
  let open Dune_lang.Decoder in
  get key >>| function
  | Some t -> t
  | None -> Code_error.raise "Current project is unset" []

let filename = "dune-project"

let implicit_transitive_deps_default ~lang:_ = true

let wrapped_executables_default ~(lang : Lang.Instance.t) =
  lang.version >= (2, 0)

let executables_implicit_empty_intf_default ~(lang : Lang.Instance.t) =
  lang.version >= (3, 0)

let strict_package_deps_default ~(lang : Lang.Instance.t) =
  lang.version >= (3, 0)

let explicit_js_mode_default ~(lang : Lang.Instance.t) = lang.version >= (2, 0)

let cram_default ~(lang : Lang.Instance.t) = lang.version >= (3, 0)

let format_extension_key =
  Extension.register Format_config.syntax Format_config.dparse_args
    Format_config.to_dyn

let format_config t =
  let ext = find_extension_args t format_extension_key in
  let dune_lang = t.format_config in
  let version = dune_version t in
  Format_config.of_config ~ext ~dune_lang ~version

let default_name ~dir ~(packages : Package.t Package.Name.Map.t) =
  match Package.Name.Map.min_binding packages with
  | None -> Name.anonymous dir
  | Some (name, pkg) -> (
    let name = Package.Name.to_string name in
    match Name.named name with
    | Some x -> x
    | None ->
      (* TODO: This is a strange error: [name] comes from a package but is
         rejected as a valid Dune project name. It would be better to make the
         set of allowed package names and the set of project names coincide. *)
      User_error.raise ~loc:pkg.loc
        [ Pp.textf "%S is not a valid Dune project name." name ])

let infer ~dir packages =
  let lang = get_dune_lang () in
  let name = default_name ~dir ~packages in
  let project_file = Path.Source.relative dir filename in
  let parsing_context, stanza_parser, extension_args =
    interpret_lang_and_extensions ~lang ~explicit_extensions:[]
  in
  let implicit_transitive_deps = implicit_transitive_deps_default ~lang in
  let wrapped_executables = wrapped_executables_default ~lang in
  let executables_implicit_empty_intf =
    executables_implicit_empty_intf_default ~lang
  in
  let explicit_js_mode = explicit_js_mode_default ~lang in
  let strict_package_deps = strict_package_deps_default ~lang in
  let cram = cram_default ~lang in
  let root = dir in
  let file_key = File_key.make ~root ~name in
  { name
  ; packages
  ; root
  ; info = Package.Info.empty
  ; version = None
  ; dune_version = lang.version
  ; implicit_transitive_deps
  ; wrapped_executables
  ; executables_implicit_empty_intf
  ; accept_alternative_dune_file_name = false
  ; stanza_parser
  ; project_file
  ; extension_args
  ; parsing_context
  ; generate_opam_files = false
  ; use_standard_c_and_cxx_flags =
      (if lang.version < (3, 0) then
        None
      else
        Some true)
  ; file_key
  ; dialects = Dialect.DB.builtin
  ; explicit_js_mode
  ; format_config = None
  ; strict_package_deps
  ; cram
  }

module Toggle = struct
  type t =
    | Enable
    | Disable

  let enabled = function
    | Enable -> true
    | Disable -> false

  let decode =
    let open Dune_lang.Decoder in
    map_validate string ~f:(function
      | "enable" -> Ok Enable
      | "disable" -> Ok Disable
      | _ -> Error (User_error.make [ Pp.text "must be 'disable' or 'enable'" ]))

  let field ?check name =
    let open Dune_lang.Decoder in
    let decode =
      match check with
      | None -> decode
      | Some check -> check >>= fun () -> decode
    in
    field_o name decode
end

let anonymous ~dir = infer ~dir Package.Name.Map.empty

let parse ~dir ~lang ~opam_packages ~file ~dir_status =
  String_with_vars.set_decoding_env
    (Pform.Env.initial lang.Lang.Instance.version)
    (fields
       (let+ name = field_o "name" Name.decode
        and+ version = field_o "version" string
        and+ info = Package.Info.decode ()
        and+ packages = multi_field "package" (Package.decode ~dir)
        and+ explicit_extensions =
          multi_field "using"
            (let+ loc = loc
             and+ name = located string
             and+ ver = located Dune_lang.Syntax.Version.decode
             and+ parse_args = capture in
             (* We don't parse the arguments quite yet as we want to set the
                version of extensions before parsing them. *)
             Extension.instantiate ~dune_lang_ver:lang.Lang.Instance.version
               ~loc ~parse_args name ver)
        and+ implicit_transitive_deps =
          field_o_b "implicit_transitive_deps"
            ~check:(Dune_lang.Syntax.since Stanza.syntax (1, 7))
        and+ wrapped_executables =
          field_o_b "wrapped_executables"
            ~check:(Dune_lang.Syntax.since Stanza.syntax (1, 11))
        and+ _allow_approx_merlin =
          (* TODO DUNE3 remove this field from parsing *)
          let+ loc = loc
          and+ f =
            field_o_b "allow_approximate_merlin"
              ~check:(Dune_lang.Syntax.since Stanza.syntax (1, 9))
          in
          let vendored =
            match dir_status with
            | Sub_dirs.Status.Vendored -> true
            | _ -> false
          in
          if
            Option.is_some f
            && Dune_lang.Syntax.Version.Infix.(lang.version >= (2, 8))
            && not vendored
          then
            Dune_lang.Syntax.Warning.deprecated_in
              ~extra_info:
                "It is useless since the Merlin configurations are not \
                 ambiguous anymore."
              loc lang.syntax (2, 8) ~what:"This field"
        and+ executables_implicit_empty_intf =
          field_o_b "executables_implicit_empty_intf"
            ~check:(Dune_lang.Syntax.since Stanza.syntax (2, 9))
        and+ accept_alternative_dune_file_name =
          field_b "accept_alternative_dune_file_name"
            ~check:(Dune_lang.Syntax.since Stanza.syntax (3, 0))
        and+ () = Dune_lang.Versioned_file.no_more_lang
        and+ generate_opam_files =
          field_o_b "generate_opam_files"
            ~check:(Dune_lang.Syntax.since Stanza.syntax (1, 10))
        and+ use_standard_c_and_cxx_flags =
          field_o_b "use_standard_c_and_cxx_flags"
            ~check:(Dune_lang.Syntax.since Stanza.syntax (2, 8))
        and+ dialects =
          multi_field "dialect"
            (Dune_lang.Syntax.since Stanza.syntax (1, 11)
            >>> located Dialect.decode)
        and+ explicit_js_mode =
          field_o_b "explicit_js_mode"
            ~check:(Dune_lang.Syntax.since Stanza.syntax (1, 11))
        and+ format_config = Format_config.field ~since:(2, 0)
        and+ strict_package_deps =
          field_o_b "strict_package_deps"
            ~check:(Dune_lang.Syntax.since Stanza.syntax (2, 3))
        and+ cram =
          Toggle.field "cram"
            ~check:(Dune_lang.Syntax.since Stanza.syntax (2, 7))
        in
        let packages =
          if List.is_empty packages then
            Package.Name.Map.map opam_packages ~f:(fun (_loc, p) ->
                Lazy.force p)
          else (
            (match (packages, name) with
            | [ p ], Some (Named name) ->
              if Package.Name.to_string (Package.name p) <> name then
                User_error.raise ~loc:p.loc
                  [ Pp.textf
                      "when a single package is defined, it must have the same \
                       name as the project name: %s"
                      name
                  ]
            | _, _ -> ());
            let package_defined_twice name loc1 loc2 =
              let main_message =
                [ Pp.textf "Package name %s is defined twice:"
                    (Package.Name.to_string name)
                ]
              in
              let name = Package.Name.to_string name in
              let annots =
                let message loc =
                  User_message.make ~loc [ Pp.textf "package named %s" name ]
                in
                let related = [ message loc1; message loc2 ] in
                [ Compound_user_error.make
                    ~main:(User_message.make main_message)
                    ~related
                ]
              in
              User_error.raise ~annots
                (main_message
                @ [ Pp.textf "- %s" (Loc.to_file_colon_line loc1)
                  ; Pp.textf "- %s" (Loc.to_file_colon_line loc2)
                  ])
            in
            let deprecated_package_names =
              List.fold_left packages ~init:Package.Name.Map.empty
                ~f:(fun acc { Package.deprecated_package_names; _ } ->
                  Package.Name.Map.union acc deprecated_package_names
                    ~f:package_defined_twice)
            in
            List.iter packages ~f:(fun p ->
                let name = Package.name p in
                match Package.Name.Map.find deprecated_package_names name with
                | None -> ()
                | Some loc -> package_defined_twice name loc p.loc);
            match
              Package.Name.Map.of_list_map packages ~f:(fun p ->
                  (Package.name p, p))
            with
            | Error (_, _, p) ->
              let name = Package.name p in
              User_error.raise ~loc:p.loc
                [ Pp.textf "package %s is already defined"
                    (Package.Name.to_string name)
                ]
            | Ok packages ->
              Package.Name.Map.merge packages opam_packages
                ~f:(fun _name dune opam ->
                  match (dune, opam) with
                  | _, None -> dune
                  | Some p, Some _ -> Some { p with has_opam_file = true }
                  | None, Some (loc, _) ->
                    User_error.raise ~loc
                      [ Pp.text
                          "This opam file doesn't have a corresponding \
                           (package ...) stanza in the dune-project_file. \
                           Since you have at least one other (package ...) \
                           stanza in your dune-project file, you must a \
                           (package ...) stanza for each opam package in your \
                           project."
                      ])
          )
        in
        let packages =
          Package.Name.Map.map packages ~f:(fun p ->
              let info = Package.Info.superpose info p.info in
              let version =
                match p.version with
                | Some _ as v -> v
                | None -> version
              in
              { p with version; info })
        in
        let name =
          match name with
          | Some n -> n
          | None -> default_name ~dir ~packages
        in
        let parsing_context, stanza_parser, extension_args =
          interpret_lang_and_extensions ~lang ~explicit_extensions
        in
        let implicit_transitive_deps =
          Option.value implicit_transitive_deps
            ~default:(implicit_transitive_deps_default ~lang)
        in
        let wrapped_executables =
          Option.value wrapped_executables
            ~default:(wrapped_executables_default ~lang)
        in
        let executables_implicit_empty_intf =
          Option.value executables_implicit_empty_intf
            ~default:(executables_implicit_empty_intf_default ~lang)
        in
        let strict_package_deps =
          Option.value strict_package_deps
            ~default:(strict_package_deps_default ~lang)
        in
        let dune_version = lang.version in
        let explicit_js_mode =
          Option.value explicit_js_mode
            ~default:(explicit_js_mode_default ~lang)
        in
        let generate_opam_files =
          Option.value ~default:false generate_opam_files
        in
        let use_standard_c_and_cxx_flags =
          match use_standard_c_and_cxx_flags with
          | None when dune_version >= (3, 0) -> Some false
          | None -> None
          | some -> some
        in
        let cram =
          match cram with
          | None -> cram_default ~lang
          | Some t -> Toggle.enabled t
        in
        let root = dir in
        let file_key = File_key.make ~name ~root in
        let dialects =
          List.fold_left
            ~f:(fun dialects (loc, dialect) ->
              Dialect.DB.add dialects ~loc dialect)
            ~init:Dialect.DB.builtin dialects
        in
        let () =
          match name with
          | Named _ -> ()
          | Anonymous _ ->
            if
              dune_version >= (2, 8)
              && generate_opam_files
              && dir_status = Sub_dirs.Status.Normal
            then
              let loc = Loc.in_file (Path.source file) in
              User_warning.emit ~loc
                [ Pp.text
                    "Project name is not specified. Add a (name \
                     <project-name>) field to your dune-project file to make \
                     sure that $ dune subst works in release or pinned builds"
                ]
        in
        { name
        ; file_key
        ; root
        ; version
        ; dune_version
        ; info
        ; packages
        ; stanza_parser
        ; project_file = file
        ; extension_args
        ; parsing_context
        ; implicit_transitive_deps
        ; wrapped_executables
        ; executables_implicit_empty_intf
        ; accept_alternative_dune_file_name
        ; generate_opam_files
        ; use_standard_c_and_cxx_flags
        ; dialects
        ; explicit_js_mode
        ; format_config
        ; strict_package_deps
        ; cram
        }))

let load_dune_project ~dir opam_packages ~dir_status =
  let file = Path.Source.relative dir filename in
  load_exn (Path.source file) ~f:(fun lang ->
      parse ~dir ~lang ~opam_packages ~file ~dir_status)

let load ~dir ~files ~infer_from_opam_files ~dir_status =
  let opam_packages =
    String.Set.fold files ~init:[] ~f:(fun fn acc ->
        match Package.Name.of_opam_file_basename fn with
        | None -> acc
        | Some name ->
          let opam_file = Path.Source.relative dir fn in
          let loc = Loc.in_file (Path.source opam_file) in
          (* We are lazy here because if the opam files are generated by dune,
             there is no point parsing them. This value will only be forced if
             generation of opam files is disabled. *)
          let pkg = lazy (Package.load_opam_file opam_file name) in
          (name, (loc, pkg)) :: acc)
    |> Package.Name.Map.of_list_exn
  in
  if String.Set.mem files filename then
    Some (load_dune_project ~dir opam_packages ~dir_status)
  else if
    Path.Source.is_root dir
    || (infer_from_opam_files && not (Package.Name.Map.is_empty opam_packages))
  then
    Some
      (infer ~dir
         (Package.Name.Map.map opam_packages ~f:(fun (_loc, p) -> Lazy.force p)))
  else
    None

let set_parsing_context t parser =
  Dune_lang.Decoder.set_many t.parsing_context parser

let wrapped_executables t = t.wrapped_executables

let executables_implicit_empty_intf t = t.executables_implicit_empty_intf

let accept_alternative_dune_file_name t = t.accept_alternative_dune_file_name

let () =
  let open Dune_lang.Decoder in
  Extension.register_simple Action_plugin.syntax (return []);
  Extension.register_simple Section.dune_site_syntax (return [])

let strict_package_deps t = t.strict_package_deps

let cram t = t.cram

let info t = t.info

let update_execution_parameters t ep =
  Execution_parameters.set_dune_version t.dune_version ep
