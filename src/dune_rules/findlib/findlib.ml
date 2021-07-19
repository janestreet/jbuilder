open! Dune_engine
open! Dune_engine.Import
open! Stdune
module Opam_package = Package
module P = Variant
module Ps = Variant.Set

let meta_fn = "META"

(* An assignment or addition *)
module Rule = struct
  type t =
    { preds_required : Ps.t
    ; preds_forbidden : Ps.t
    ; value : string
    }

  let to_dyn { preds_required; preds_forbidden; value } =
    let open Dyn.Encoder in
    record
      [ ("preds_required", Ps.to_dyn preds_required)
      ; ("preds_forbidden", Ps.to_dyn preds_forbidden)
      ; ("value", string value)
      ]

  let formal_predicates_count t =
    Ps.cardinal t.preds_required + Ps.cardinal t.preds_forbidden

  let matches t ~preds =
    Ps.is_subset t.preds_required ~of_:preds
    && Ps.is_empty (Ps.inter preds t.preds_forbidden)

  let make (rule : Meta.rule) =
    let preds_required, preds_forbidden =
      List.partition_map rule.predicates ~f:(function
        | Pos x -> Left x
        | Neg x -> Right x)
    in
    { preds_required = Ps.make preds_required
    ; preds_forbidden = Ps.make preds_forbidden
    ; value = rule.value
    }
end

(* Set of rules for a given variable of a package. Implements the algorithm
   described here:

   http://projects.camlcity.org/projects/dl/findlib-1.6.3/doc/ref-html/r729.html *)
module Rules = struct
  (* To implement the algorithm, [set_rules] is sorted by decreasing number of
     formal predicates, then according to the order of the META file.
     [add_rules] are in the same order as in the META file. *)
  type t =
    { set_rules : Rule.t list
    ; add_rules : Rule.t list
    }

  let to_dyn { set_rules; add_rules } =
    let open Dyn.Encoder in
    record
      [ ("set_rules", list Rule.to_dyn set_rules)
      ; ("add_rules", list Rule.to_dyn add_rules)
      ]

  let interpret t ~preds =
    let rec find_set_rule = function
      | [] -> None
      | rule :: rules ->
        if Rule.matches rule ~preds then
          Some rule.value
        else
          find_set_rule rules
    in
    let v = find_set_rule t.set_rules in
    List.fold_left t.add_rules ~init:v ~f:(fun v rule ->
        if Rule.matches rule ~preds then
          Some (Option.value ~default:"" v ^ " " ^ rule.value)
        else
          v)

  let of_meta_rules (rules : Meta.Simplified.Rules.t) =
    let add_rules = List.map rules.add_rules ~f:Rule.make in
    let set_rules =
      List.map rules.set_rules ~f:Rule.make
      |> List.stable_sort ~compare:(fun a b ->
             Poly.compare
               (Rule.formal_predicates_count b)
               (Rule.formal_predicates_count a))
    in
    { add_rules; set_rules }
end

module Vars = struct
  type t = Rules.t String.Map.t

  let get (t : t) var preds =
    Option.map (String.Map.find t var) ~f:(fun r ->
        Option.value ~default:"" (Rules.interpret r ~preds))

  let get_words t var preds =
    match get t var preds with
    | None -> []
    | Some s -> String.extract_comma_space_separated_words s
end

module Config = struct
  type t =
    { vars : Vars.t
    ; preds : Ps.t
    }

  let to_dyn { vars; preds } =
    let open Dyn.Encoder in
    record
      [ ("vars", String.Map.to_dyn Rules.to_dyn vars)
      ; ("preds", Ps.to_dyn preds)
      ]

  let load path ~toolchain ~context =
    let path = Path.extend_basename path ~suffix:".d" in
    let conf_file = Path.relative path (toolchain ^ ".conf") in
    if not (Path.exists conf_file) then
      User_error.raise
        [ Pp.textf "ocamlfind toolchain %s isn't defined in %s (context: %s)"
            toolchain
            (Path.to_string_maybe_quoted path)
            context
        ];
    let vars = (Meta.load ~name:None conf_file).vars in
    { vars = String.Map.map vars ~f:Rules.of_meta_rules
    ; preds = Ps.make [ toolchain ]
    }

  let get { vars; preds } var = Vars.get vars var preds

  let env t =
    let preds = Ps.add t.preds (P.make "env") in
    String.Map.filter_map ~f:(Rules.interpret ~preds) t.vars
    |> Env.of_string_map
end

module Unavailable_reason = struct
  type t =
    | Not_found
    | Invalid_dune_package of exn

  let to_dyn =
    let open Dyn.Encoder in
    function
    | Not_found -> constr "Not_found" []
    | Invalid_dune_package why ->
      constr "Invalid_dune_package" [ Exn.to_dyn why ]
end

let builtin_for_dune : Dune_package.t =
  let entry =
    Dune_package.Entry.Deprecated_library_name
      { loc = Loc.of_pos __POS__
      ; old_public_name = Lib_name.of_string "dune.configurator"
      ; new_public_name = Lib_name.of_string "dune-configurator"
      }
  in
  { name = Opam_package.Name.of_string "dune"
  ; entries = Lib_name.Map.singleton (Dune_package.Entry.name entry) entry
  ; version = None
  ; dir = Path.root
  ; sections = Section.Map.empty
  ; sites = Section.Site.Map.empty
  ; files = []
  }

type db =
  { stdlib_dir : Path.t
  ; paths : Path.t list
  ; builtins : Meta.Simplified.t Package.Name.Map.t
  ; root_packages :
      (Package.Name.t, (Dune_package.t, Unavailable_reason.t) result) Table.t
  ; lib_config : Lib_config.t
  }

let builtins db = db.builtins

let paths t = t.paths

let findlib_predicates_set_by_dune =
  Ps.of_list [ P.ppx_driver; P.mt; P.mt_posix ]

module Loader : sig
  (* Search for a <package>/{META,dune-package} file in the findlib search
     path *)
  val lookup_and_load :
    db -> Package.Name.t -> (Dune_package.t, Unavailable_reason.t) result

  val dummy_package : db -> Lib_name.t -> Dune_package.t
end = struct
  module Findlib_package : sig
    type t =
      { meta_file : Path.t
      ; name : Lib_name.t
      ; dir : Path.t
      ; vars : Vars.t
      }

    val to_dune_library : t -> lib_config:Lib_config.t -> Dune_package.Lib.t

    val exists : t -> is_builtin:bool -> bool
  end = struct
    type t =
      { meta_file : Path.t
      ; name : Lib_name.t
      ; dir : Path.t
      ; vars : Vars.t
      }

    let preds = findlib_predicates_set_by_dune

    let get_paths t var preds =
      List.map (Vars.get_words t.vars var preds) ~f:(Path.relative t.dir)

    let make_archives t var preds =
      Mode.Dict.of_func (fun ~mode ->
          get_paths t var (Ps.add preds (Mode.variant mode)))

    let version t = Vars.get t.vars "version" Ps.empty

    let description t = Vars.get t.vars "description" Ps.empty

    let jsoo_runtime t = get_paths t "jsoo_runtime" Ps.empty

    let requires t =
      Vars.get_words t.vars "requires" preds
      |> List.map ~f:(fun s -> Lib_name.parse_string_exn (Loc.none, s))

    let ppx_runtime_deps t =
      Vars.get_words t.vars "ppx_runtime_deps" preds
      |> List.map ~f:(fun s -> Lib_name.parse_string_exn (Loc.none, s))

    let kind t =
      match Vars.get t.vars "library_kind" Ps.empty with
      | None -> Lib_kind.Normal
      | Some "ppx_rewriter" -> Ppx_rewriter Lib_kind.Ppx_args.empty
      | Some "ppx_deriver" -> Ppx_deriver Lib_kind.Ppx_args.empty
      | Some _other_string -> Lib_kind.Normal

    let archives t = make_archives t "archive" preds

    let plugins t =
      Mode.Dict.map2 ~f:( @ )
        (make_archives t "archive" (Ps.add preds Variant.plugin))
        (make_archives t "plugin" preds)

    let mangled_module_re =
      lazy
        (let open Re in
        [ rep any; str "__"; rep any ] |> seq |> compile)

    let exists t ~is_builtin =
      let exists_if = Vars.get_words t.vars "exists_if" Ps.empty in
      match exists_if with
      | _ :: _ ->
        List.for_all exists_if ~f:(fun fn ->
            Path.exists (Path.relative t.dir fn))
      | [] -> (
        (not is_builtin)
        ||
        (* The META files for installed packages are sometimes broken, i.e. META
           files for libraries that were not installed by the compiler are still
           present:

           https://github.com/ocaml/dune/issues/563

           To workaround this problem, for builtin packages we check that at
           least one of the archive is present. *)
        match archives t with
        | { byte = []; native = [] } -> true
        | { byte; native } -> List.exists (byte @ native) ~f:Path.exists)

    let to_dune_library t ~(lib_config : Lib_config.t) =
      let loc = Loc.in_file t.meta_file in
      let add_loc x = (loc, x) in
      let dot_dune_file =
        Path.relative t.dir (sprintf "%s.dune" (Lib_name.to_string t.name))
      in
      if Path.exists dot_dune_file then
        User_warning.emit
          ~loc:(Loc.in_file dot_dune_file)
          [ Pp.text
              ".dune files are ignored since 2.0. Reinstall the library with \
               dune >= 2.0 to get rid of this warning and enable support for \
               the subsystem this library provides."
          ];
      let archives = archives t in
      let obj_dir = Obj_dir.make_external_no_private ~dir:t.dir in
      let modes : Mode.Dict.Set.t =
        (* libraries without archives are compatible with all modes. mainly a
           hack for compiler-libs which doesn't have any archives *)
        let discovered = Mode.Dict.map ~f:List.is_non_empty archives in
        if Mode.Dict.Set.is_empty discovered then
          Mode.Dict.Set.all
        else
          discovered
      in
      let info : Path.t Lib_info.t =
        let kind = kind t in
        let sub_systems = Sub_system_name.Map.empty in
        let synopsis = description t in
        let status =
          match Lib_name.analyze t.name with
          | Private (_, _) -> Lib_info.Status.Installed_private
          | Public (_, _) -> Lib_info.Status.Installed
        in
        let src_dir = Obj_dir.dir obj_dir in
        let version = version t in
        let dune_version = None in
        let virtual_deps = [] in
        let implements = None in
        let orig_src_dir = None in
        let main_module_name : Lib_info.Main_module_name.t = This None in
        let enabled = Lib_info.Enabled_status.Normal in
        let requires =
          requires t |> List.map ~f:(fun name -> Lib_dep.direct (add_loc name))
        in
        let ppx_runtime_deps = List.map ~f:add_loc (ppx_runtime_deps t) in
        let special_builtin_support : Lib_info.Special_builtin_support.t option
            =
          (* findlib has been around for much longer than dune, so it is
             acceptable to have a special case in dune for findlib. *)
          match Lib_name.to_string t.name with
          | "findlib.dynload" -> Some Findlib_dynload
          | _ -> None
        in
        let foreign_objects = Lib_info.Source.External [] in
        let plugins = plugins t in
        let jsoo_runtime = jsoo_runtime t in
        let jsoo_archive = None in
        let preprocess = Preprocess.Per_module.no_preprocessing () in
        let virtual_ = None in
        let default_implementation = None in
        let wrapped = None in
        let dir_contents = Path.readdir_unsorted t.dir in
        let foreign_archives, native_archives =
          (* Here we scan [t.dir] and consider all files named [lib*.ext_lib] to
             be foreign archives, and all other files with the extension
             [ext_lib] to be native archives. The resulting lists of archives
             will be used to compute appropriate flags for linking dependent
             executables. *)
          match dir_contents with
          | Error _ ->
            (* Raising an error is not an option here as we systematically delay
               all library loading errors until the libraries are actually used
               in rules.

               We could add a warning like this:

               User_warning.emit ~loc:(Loc.in_dir t.dir) [ Pp.text "Unable to
               read directory" ];

               But it seems to be too invasive *)
            ([], [])
          | Ok res ->
            let foreign_archives, native_archives =
              List.rev_filter_partition_map res ~f:(fun f ->
                  let ext = Filename.extension f in
                  if ext = lib_config.ext_lib then
                    let file = Path.relative t.dir f in
                    if
                      String.is_prefix f
                        ~prefix:Foreign.Archive.Name.lib_file_prefix
                    then
                      Left file
                    else
                      Right file
                  else
                    Skip)
            in
            let sort = List.sort ~compare:Path.compare in
            (sort foreign_archives, sort native_archives)
        in
        let entry_modules =
          Lib_info.Source.External
            (match Vars.get_words t.vars "main_modules" Ps.empty with
            | _ :: _ as modules ->
              Ok (List.map ~f:Module_name.of_string modules)
            | [] -> (
              match dir_contents with
              | Error e ->
                Error
                  (User_error.E
                     ( User_message.make
                         [ Pp.textf "Unable to get entry modules of %s in %s. "
                             (Lib_name.to_string t.name)
                             (Path.to_string src_dir)
                         ; Pp.textf "error: %s" (Unix.error_message e)
                         ]
                     , [] ))
              | Ok files ->
                let ext = Cm_kind.ext Cmi in
                Result.List.filter_map files ~f:(fun fname ->
                    match Filename.check_suffix fname ext with
                    | false -> Ok None
                    | true -> (
                      if
                        (* We add this hack to skip manually mangled
                           libraries *)
                        Re.execp (Lazy.force mangled_module_re) fname
                      then
                        Ok None
                      else
                        match
                          let name = Filename.chop_extension fname in
                          Module_name.of_string_user_error
                            (Loc.in_dir src_dir, name)
                        with
                        | Ok s -> Ok (Some s)
                        | Error e -> Error (User_error.E (e, []))))))
        in
        Lib_info.create ~path_kind:External ~loc ~name:t.name ~kind ~status
          ~src_dir ~orig_src_dir ~obj_dir ~version ~synopsis ~main_module_name
          ~sub_systems ~requires ~foreign_objects ~plugins ~archives
          ~ppx_runtime_deps ~foreign_archives
          ~native_archives:(Files native_archives) ~foreign_dll_files:[]
          ~jsoo_runtime ~jsoo_archive ~preprocess ~enabled ~virtual_deps
          ~dune_version ~virtual_ ~implements ~default_implementation ~modes
          ~wrapped ~special_builtin_support ~exit_module:None
          ~instrumentation_backend:None ~entry_modules
      in
      Dune_package.Lib.of_findlib info
  end

  (* Parse all the packages defined in a META file *)
  let dune_package_of_meta db ~dir ~meta_file ~(meta : Meta.Simplified.t) =
    let rec loop ~dir ~full_name (meta : Meta.Simplified.t) acc =
      let vars = String.Map.map meta.vars ~f:Rules.of_meta_rules in
      let pkg_dir = Vars.get vars "directory" Ps.empty in
      let dir =
        match pkg_dir with
        | None
        | Some "" ->
          dir
        | Some pkg_dir ->
          if pkg_dir.[0] = '+' || pkg_dir.[0] = '^' then
            Path.relative db.stdlib_dir (String.drop pkg_dir 1)
          else if Filename.is_relative pkg_dir then
            Path.relative dir pkg_dir
          else
            Path.of_filename_relative_to_initial_cwd pkg_dir
      in
      let pkg : Findlib_package.t =
        { meta_file; name = full_name; dir; vars }
      in
      let lib = Findlib_package.to_dune_library pkg ~lib_config:db.lib_config in
      let entry : Dune_package.Entry.t =
        if
          Findlib_package.exists pkg
            ~is_builtin:
              (Package.Name.Map.mem db.builtins
                 (Lib_name.package_name pkg.name))
        then
          Library lib
        else
          Hidden_library lib
      in
      let acc =
        Lib_name.Map.add_exn acc (Dune_package.Entry.name entry) entry
      in
      List.fold_left meta.subs ~init:acc
        ~f:(fun acc (meta : Meta.Simplified.t) ->
          let full_name =
            match meta.name with
            | None -> full_name
            | Some name -> Lib_name.nest full_name name
          in
          loop ~dir ~full_name meta acc)
    in
    let name = Option.value_exn meta.name in
    let entries =
      loop ~dir ~full_name:(Option.value_exn meta.name) meta Lib_name.Map.empty
    in
    { Dune_package.name = Lib_name.package_name name
    ; version =
        (let open Option.O in
        let* e = Lib_name.Map.find entries name in
        Dune_package.Entry.version e)
    ; entries
    ; dir
    ; sections = Section.Map.empty
    ; sites = Section.Site.Map.empty
    ; files = []
    }

  let load_and_convert db ~dir ~meta_file ~name =
    let meta = Meta.load meta_file ~name:(Some name) in
    dune_package_of_meta db ~dir ~meta_file ~meta

  let load_builtin db meta =
    dune_package_of_meta db ~dir:db.stdlib_dir
      ~meta_file:(Path.of_string "<internal>")
      ~meta

  let dummy_package db lib_name =
    let pkg, names = Lib_name.split lib_name in
    let top_lib = Lib_name.of_package_name pkg in
    let dummy name subs =
      { Meta.Simplified.name = Some name; vars = String.Map.empty; subs }
    in
    let subs : Meta.Simplified.t list =
      let rec loop = function
        | [] -> []
        | name :: names -> [ dummy (Lib_name.of_string name) (loop names) ]
      in
      loop names
    in
    let meta = dummy top_lib subs in
    load_builtin db meta

  let lookup_and_load_one_dir db ~dir ~name =
    let meta_file = Path.relative dir meta_fn in
    if Path.exists meta_file then
      Some (load_and_convert db ~dir ~meta_file ~name)
    else
      (* Alternative layout *)
      let open Option.O in
      let* dir = Path.parent dir in
      let meta_file =
        Path.relative dir (meta_fn ^ "." ^ Package.Name.to_string name)
      in
      if Path.exists meta_file then
        Some (load_and_convert db ~dir ~meta_file ~name)
      else
        None

  let lookup_and_load db name =
    let rec loop dirs : (Dune_package.t, Unavailable_reason.t) Result.t =
      match dirs with
      | [] -> (
        match Package.Name.to_string name with
        | "dune" -> Ok builtin_for_dune
        | _ -> (
          Package.Name.Map.find db.builtins name |> function
          | None -> Error Unavailable_reason.Not_found
          | Some meta -> Ok (load_builtin db meta)))
      | dir :: dirs -> (
        let dir = Path.relative dir (Package.Name.to_string name) in
        let dune = Path.relative dir Dune_package.fn in
        match
          if Path.exists dune then
            Dune_package.Or_meta.load dune
          else
            Ok Dune_package.Or_meta.Use_meta
        with
        | Error e -> Error (Unavailable_reason.Invalid_dune_package e)
        | Ok (Dune_package p) -> Ok p
        | Ok Use_meta -> (
          match lookup_and_load_one_dir db ~dir ~name with
          | None -> loop dirs
          | Some p -> Ok p))
    in
    loop db.paths
end

type t = db

let dummy_lib t ~name =
  let p = Loader.dummy_package t name in
  match Lib_name.Map.find_exn p.entries name with
  | Library lib -> lib
  | _ -> assert false

let find_root_package t name =
  match Table.find t.root_packages name with
  | Some x -> x
  | None ->
    let res = Loader.lookup_and_load t name in
    Table.set t.root_packages name res;
    res

let find t name =
  let open Result.O in
  let* p = find_root_package t (Lib_name.package_name name) in
  match Lib_name.Map.find p.entries name with
  | Some x -> Ok x
  | None -> Error Unavailable_reason.Not_found

let available t name = Result.is_ok (find t name)

let root_packages t =
  let pkgs =
    List.concat_map t.paths ~f:(fun dir ->
        match Path.readdir_unsorted dir with
        | Error ENOENT -> []
        | Error unix_error ->
          User_error.raise
            [ Pp.textf "Unable to read directory %s for findlib package"
                (Path.to_string_maybe_quoted dir)
            ; Pp.textf "Reason: %s" (Unix.error_message unix_error)
            ]
        | Ok listing ->
          List.filter_map listing ~f:(fun name ->
              if Path.exists (Path.relative dir (name ^ "/" ^ meta_fn)) then
                Some (Package.Name.of_string name)
              else
                None))
    |> Package.Name.Set.of_list
  in
  let builtins = Package.Name.Set.of_list (Package.Name.Map.keys t.builtins) in
  Package.Name.Set.union pkgs builtins

let load_all_packages t =
  Package.Name.Set.iter (root_packages t) ~f:(fun name ->
      ignore (find_root_package t name))

let all_packages t =
  load_all_packages t;
  Table.fold t.root_packages ~init:[] ~f:(fun x acc ->
      match x with
      | Ok p -> Lib_name.Map.fold p.entries ~init:acc ~f:(fun x acc -> x :: acc)
      | Error _ -> acc)
  |> List.sort ~compare:(fun a b ->
         Lib_name.compare
           (Dune_package.Entry.name a)
           (Dune_package.Entry.name b))

(* CR-someday amokhov: Remove the mutable table below and add:

   - A memoized function for finding packages by names (see [find]).

   - A [Memo.Lazy.Async.t] storing the set of all packages (see
   [root_packages]). *)
let create ~paths ~(lib_config : Lib_config.t) =
  let stdlib_dir = lib_config.stdlib_dir in
  let version = lib_config.ocaml_version in
  { stdlib_dir
  ; paths
  ; builtins = Meta.builtins ~stdlib_dir ~version
  ; root_packages = Table.create (module Package.Name) 1024
  ; lib_config
  }

let all_broken_packages t =
  load_all_packages t;
  Table.foldi t.root_packages ~init:[] ~f:(fun name x acc ->
      match x with
      | Ok _
      | Error Not_found ->
        acc
      | Error (Invalid_dune_package exn) -> (name, exn) :: acc)
  |> List.sort ~compare:(fun (a, _) (b, _) -> Package.Name.compare a b)
