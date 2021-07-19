open! Dune_engine
open! Stdune

module Meta_parser = Dune_meta_parser.Meta_parser.Make (struct
  include Stdune
  module Lib_name = Lib_name
end)

include Meta_parser

let dyn_of_action =
  let open Dyn.Encoder in
  function
  | Set -> constr "Set" []
  | Add -> constr "Add" []

let dyn_of_predicate =
  let open Dyn.Encoder in
  function
  | Pos s -> constr "Pos" [ String s ]
  | Neg s -> constr "Neg" [ String s ]

let dyn_of_rule { var; predicates; action; value } =
  let open Dyn.Encoder in
  record
    [ ("var", string var)
    ; ("predicates", list dyn_of_predicate predicates)
    ; ("action", dyn_of_action action)
    ; ("value", string value)
    ]

let rec dyn_of_entry (entry : entry) =
  let open Dyn.Encoder in
  match entry with
  | Comment c -> constr "Comment" [ string c ]
  | Rule r -> constr "Rule" [ dyn_of_rule r ]
  | Package p -> constr "Package" [ to_dyn p ]

and to_dyn { name; entries } =
  let open Dyn.Encoder in
  record
    [ ("name", option Lib_name.to_dyn name)
    ; ("entries", list dyn_of_entry entries)
    ]

let rec filter_variable t ~f =
  { t with
    entries =
      List.filter_map t.entries ~f:(function
        | Comment _ -> None
        | Rule { var; _ } as r when f var -> Some r
        | Rule _ -> None
        | Package t -> Some (Package (filter_variable ~f t)))
  }

module Simplified = struct
  module Rules = struct
    type t =
      { set_rules : rule list
      ; add_rules : rule list
      }

    let to_dyn { set_rules; add_rules } =
      let open Dyn.Encoder in
      record
        [ ("set_rules", list dyn_of_rule set_rules)
        ; ("add_rules", list dyn_of_rule add_rules)
        ]
  end

  type t =
    { name : Lib_name.t option
    ; vars : Rules.t String.Map.t
    ; subs : t list
    }

  let rec to_dyn { name; vars; subs } =
    let open Dyn.Encoder in
    record
      [ ("name", option Lib_name.to_dyn name)
      ; ("vars", String.Map.to_dyn Rules.to_dyn vars)
      ; ("subs", list to_dyn subs)
      ]
end

let rec simplify t =
  List.fold_right t.entries
    ~init:{ name = t.name; vars = String.Map.empty; subs = [] }
    ~f:(fun entry (pkg : Simplified.t) ->
      match entry with
      | Comment _ -> pkg
      | Package sub -> { pkg with subs = simplify sub :: pkg.subs }
      | Rule rule ->
        let rules =
          Option.value
            (String.Map.find pkg.vars rule.var)
            ~default:{ set_rules = []; add_rules = [] }
        in
        let rules =
          match rule.action with
          | Set -> { rules with set_rules = rule :: rules.set_rules }
          | Add -> { rules with add_rules = rule :: rules.add_rules }
        in
        { pkg with vars = String.Map.set pkg.vars rule.var rules })

let rec complexify t =
  let rules =
    List.concat_map (String.Map.values t.Simplified.vars) ~f:(fun rules ->
        let mk r = Rule r in
        List.map ~f:mk rules.set_rules @ List.map ~f:mk rules.add_rules)
  in
  let pkgs =
    List.map t.Simplified.subs ~f:(fun pkg -> Package (complexify pkg))
  in
  { name = t.Simplified.name; entries = rules @ pkgs }

let parse_entries lb = Parse.entries lb 0 []

let load p ~name =
  let name = Option.map name ~f:Lib_name.of_package_name in
  { name; entries = Io.with_lexbuf_from_file p ~f:parse_entries } |> simplify

let rule var predicates action value = Rule { var; predicates; action; value }

let requires ?(preds = []) pkgs =
  rule "requires" preds Set (String.concat ~sep:" " pkgs)

let version s = rule "version" [] Set s

let directory s = rule "directory" [] Set s

let archive p s = rule "archive" [ Pos p ] Set s

let plugin p s = rule "plugin" [ Pos p ] Set s

let exists_if s = rule "exists_if" [] Set s

let archives ?(kind = [ Mode.Byte; Mode.Native ]) name =
  List.filter_map
    [ (Mode.Byte, archive, Mode.compiled_lib_ext)
    ; (Mode.Native, archive, Mode.compiled_lib_ext)
    ; (Mode.Byte, plugin, Mode.compiled_lib_ext)
    ; (Mode.Native, plugin, Mode.plugin_ext)
    ] ~f:(fun (k, f, ext) ->
      if List.mem kind k ~equal:Mode.equal then
        Some (f (Mode.to_string k) (name ^ ext k))
      else
        None)

(* fake entry we use to pass down the list of toplevel modules for
   root_module *)
let main_modules names =
  List.map ~f:String.capitalize_ascii names
  |> String.concat ~sep:" " |> rule "main_modules" [] Set

let builtins ~stdlib_dir ~version:ocaml_version =
  let version = version "[distributed with Ocaml]" in
  let simple name ?(labels = false) ?dir ?archive_name ?kind ?exists_if_ext deps
      =
    let archive_name =
      match archive_name with
      | None -> name
      | Some a -> a
    in
    let main_modules =
      let name =
        String.map name ~f:(function
          | '-' -> '_'
          | c -> c)
      in
      if labels then
        main_modules [ name; name ^ "Labels" ]
      else
        main_modules [ name ]
    in
    let name = Lib_name.of_string name in
    let archives = archives archive_name ?kind in
    let main_modules = main_modules in
    { name = Some name
    ; entries =
        requires deps :: version :: main_modules
        ::
        (match dir with
        | None -> []
        | Some d -> [ directory d ])
        @ (match exists_if_ext with
          | None -> []
          | Some ext -> [ exists_if (archive_name ^ ext) ])
        @ archives
    }
  in
  let dummy name =
    { name = Some (Lib_name.of_string name)
    ; entries = [ version; main_modules [ name ] ]
    }
  in
  let compiler_libs =
    let sub name ?kind ?exists_if_ext deps =
      Package
        (simple name deps ~archive_name:("ocaml" ^ name) ?kind ?exists_if_ext)
    in
    { name = Some (Lib_name.of_string "compiler-libs")
    ; entries =
        [ requires []
        ; version
        ; directory "+compiler-libs"
        ; sub "common" []
        ; sub "bytecomp" [ "compiler-libs.common" ]
        ; sub "optcomp" [ "compiler-libs.common" ]
        ; sub "toplevel" [ "compiler-libs.bytecomp" ] ~kind:[ Byte ]
        ; sub "native-toplevel"
            [ "compiler-libs.optcomp"; "dynlink" ]
            ~kind:[ Native ]
            ~exists_if_ext:(Mode.compiled_lib_ext Native)
        ]
    }
  in
  let stdlib = dummy "stdlib" in
  let str = simple "str" [] ~dir:"+" in
  let unix = simple ~labels:true "unix" [] ~dir:"+" in
  let bigarray =
    if
      Ocaml_version.stdlib_includes_bigarray ocaml_version
      && not (Path.exists (Path.relative stdlib_dir "bigarray.cma"))
    then
      dummy "bigarray"
    else
      simple "bigarray" [ "unix" ] ~dir:"+"
  in
  let dynlink = simple "dynlink" [] ~dir:"+" in
  let bytes = dummy "bytes" in
  let result = dummy "result" in
  let uchar = dummy "uchar" in
  let seq = dummy "seq" in
  let threads =
    { name = Some (Lib_name.of_string "threads")
    ; entries =
        [ version
        ; main_modules [ "thread" ]
        ; requires ~preds:[ Pos "mt"; Pos "mt_vm" ] [ "threads.vm" ]
        ; requires ~preds:[ Pos "mt"; Pos "mt_posix" ] [ "threads.posix" ]
        ; directory "+"
        ; rule "type_of_threads" [] Set "posix"
        ; rule "error" [ Neg "mt" ] Set "Missing -thread or -vmthread switch"
        ; rule "error"
            [ Neg "mt_vm"; Neg "mt_posix" ]
            Set "Missing -thread or -vmthread switch"
        ; Package
            (simple "vm" [ "unix" ] ~dir:"+vmthreads" ~archive_name:"threads")
        ; Package
            (simple "posix" [ "unix" ] ~dir:"+threads" ~archive_name:"threads")
        ]
    }
  in
  let num =
    { name = Some (Lib_name.of_string "num")
    ; entries =
        [ requires [ "num.core" ]
        ; main_modules [ "num" ]
        ; version
        ; Package (simple "core" [] ~dir:"+" ~archive_name:"nums")
        ]
    }
  in
  let graphics = simple "graphics" [] ~dir:"+" in
  let ocamldoc =
    simple "ocamldoc" [ "compiler-libs" ] ~dir:"+ocamldoc" ~kind:[]
  in
  let libs =
    let base =
      [ stdlib
      ; compiler_libs
      ; str
      ; unix
      ; bigarray
      ; threads
      ; dynlink
      ; bytes
      ; ocamldoc
      ]
    in
    let base =
      if Ocaml_version.pervasives_includes_result ocaml_version then
        result :: base
      else
        base
    in
    let base =
      if Ocaml_version.stdlib_includes_uchar ocaml_version then
        uchar :: base
      else
        base
    in
    let base =
      if Ocaml_version.stdlib_includes_seq ocaml_version then
        seq :: base
      else
        base
    in
    let base =
      if Path.exists (Path.relative stdlib_dir "graphics.cma") then
        graphics :: base
      else
        base
    in
    (* We do not rely on an "exists_if" ocamlfind variable, because it would
       produce an error message mentioning a "hidden" package (which could be
       confusing). *)
    if Path.exists (Path.relative stdlib_dir "nums.cma") then
      num :: base
    else
      base
  in
  List.filter_map libs ~f:(fun t ->
      Option.map t.name ~f:(fun name ->
          (Lib_name.package_name name, simplify t)))
  |> Package.Name.Map.of_list_exn

let string_of_action = function
  | Set -> "="
  | Add -> "+="

let pp_predicate p =
  Pp.verbatim
    (match p with
    | Pos p -> p
    | Neg p -> "-" ^ p)

let pp_print_text s =
  let open Pp.O in
  Pp.verbatim "\""
  ++ Pp.hvbox (Pp.text (String.escape_only '"' s))
  ++ Pp.verbatim "\""

let pp_print_string s =
  let open Pp.O in
  Pp.verbatim "\""
  ++ Pp.hvbox (Pp.verbatim (String.escape_only '"' s))
  ++ Pp.verbatim "\""

let pp_quoted_value var =
  match var with
  | "archive"
  | "plugin"
  | "requires"
  | "ppx_runtime_deps"
  | "linkopts"
  | "jsoo_runtime" ->
    pp_print_text
  | _ -> pp_print_string

let rec pp entries = Pp.vbox (Pp.concat_map entries ~sep:Pp.newline ~f:pp_entry)

and pp_entry entry =
  match entry with
  | Comment s -> Pp.verbatim (sprintf "# %s" s)
  | Rule { var; predicates = []; action; value } ->
    Pp.box
      (Pp.concat ~sep:Pp.space
         [ Pp.verbatim var
         ; Pp.verbatim (string_of_action action)
         ; pp_quoted_value var value
         ])
  | Rule { var; predicates; action; value } ->
    let open Pp.O in
    Pp.box
      (Pp.concat ~sep:(Pp.verbatim " ")
         [ Pp.textf "%s(" var
           ++ Pp.concat_map predicates ~sep:(Pp.verbatim ",") ~f:pp_predicate
           ++ Pp.verbatim ")"
         ; Pp.verbatim (string_of_action action)
         ; pp_quoted_value var value
         ])
  | Package { name; entries } ->
    let name =
      match name with
      | None -> ""
      | Some l -> Lib_name.to_string l
    in
    let open Pp.O in
    Pp.vbox ~indent:2
      (Pp.verbatim (sprintf "package %S (" name) ++ Pp.cut ++ pp entries)
    ++ Pp.cut ++ Pp.verbatim ")"
