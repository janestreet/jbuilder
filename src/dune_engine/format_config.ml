open! Stdune
open Import
open Dune_lang.Decoder

let syntax =
  Dune_lang.Syntax.create ~name:"fmt"
    ~desc:"integration with automatic formatters"
    [ ((1, 0), `Since (1, 4))
    ; ((1, 1), `Since (1, 7))
    ; ((1, 2), `Since (1, 11))
    ]

module Language = struct
  module Key = struct
    type t =
      | Dialect of string
      | Dune

    let compare t1 t2 =
      match (t1, t2) with
      | Dune, Dune -> Eq
      | Dune, Dialect _ -> Lt
      | Dialect _, Dune -> Gt
      | Dialect s1, Dialect s2 -> String.compare s1 s2

    let to_dyn =
      let open Dyn.Encoder in
      function
      | Dialect name -> constr "dialect" [ string name ]
      | Dune -> constr "dune" []
  end

  module Map = Map.Make (Key)
  module Set = Set.Make (Key) (Map)
  include Key

  let of_string = function
    | "dune" -> Dune
    | s -> Dialect s

  let in_ext_1_0 = Set.of_list [ Dialect "ocaml"; Dialect "reason" ]

  let in_ext_1_1 = Set.add in_ext_1_0 Dune

  let encode =
    let open Dune_lang.Encoder in
    function
    | Dune -> string "dune"
    | Dialect d -> string d
end

module Enabled_for = struct
  type t =
    | Only of Language.Set.t
    | All

  let to_dyn =
    let open Dyn.Encoder in
    function
    | Only l -> constr "only" [ Language.Set.to_dyn l ]
    | All -> string "all"

  let includes t =
    match t with
    | Only l -> Language.Set.mem l
    | All -> fun _ -> true

  let field = field_o "enabled_for" (repeat (map ~f:Language.of_string string))

  let field_ext =
    let+ list_opt = field
    and+ ext_version = Dune_lang.Syntax.get_exn syntax in
    match (list_opt, ext_version) with
    | Some l, _ -> Only (Language.Set.of_list l)
    | None, (1, 0) -> Only Language.in_ext_1_0
    | None, (1, 1) -> Only Language.in_ext_1_1
    | None, (1, 2) -> All
    | None, _ ->
      Code_error.raise "This fmt version does not exist"
        [ ("version", Dune_lang.Syntax.Version.to_dyn ext_version) ]

  let equal t1 t2 =
    match (t1, t2) with
    | All, All -> true
    | Only l1, Only l2 -> Language.Set.equal l1 l2
    | _ -> false
end

type 'enabled_for generic_t =
  { loc : Loc.t
  ; enabled_for : 'enabled_for
  }

type t = Enabled_for.t generic_t

let includes t lang = Enabled_for.includes t.enabled_for lang

let to_dyn { enabled_for; loc = _ } =
  let open Dyn.Encoder in
  record [ ("enabled_for", Enabled_for.to_dyn enabled_for) ]

let dparse_args =
  let+ loc = loc
  and+ enabled_for = fields Enabled_for.field_ext in
  ({ loc; enabled_for }, [])

let dune2_record_syntax =
  let+ ef = Enabled_for.field in
  match ef with
  | Some l -> Enabled_for.Only (Language.Set.of_list l)
  | None -> All

let dune2_dec =
  let+ loc = loc
  and+ enabled_for =
    keyword "disabled"
    >>> return (Enabled_for.Only Language.Set.empty)
    <|> fields dune2_record_syntax
  in
  { loc; enabled_for }

let enabled_for_all = { loc = Loc.none; enabled_for = Enabled_for.All }

let disabled =
  { loc = Loc.none; enabled_for = Enabled_for.Only Language.Set.empty }

let field ~since =
  field_o "formatting" (Dune_lang.Syntax.since Stanza.syntax since >>> dune2_dec)

let is_empty = function
  | { enabled_for = Enabled_for.Only l; _ } -> Language.Set.is_empty l
  | { enabled_for = All; _ } -> false

let loc t = t.loc

let encode_formatting enabled_for =
  let open Dune_lang.Encoder in
  record_fields
    [ field_i "enabled_for"
        (List.map ~f:Language.encode)
        (Language.Set.to_list enabled_for)
    ]

let encode_explicit conf =
  let open Dune_lang.Encoder in
  [ field_i "formatting" encode_formatting conf ] |> record_fields |> List.hd

let to_explicit { loc; enabled_for } =
  match enabled_for with
  | Enabled_for.All -> None
  | Only l -> Some { loc; enabled_for = l }

let of_config ~ext ~dune_lang ~version =
  let dune2 = version >= (2, 0) in
  match (ext, dune_lang, dune2) with
  | None, None, true -> enabled_for_all
  | None, None, false -> disabled
  | Some x, None, false
  | None, Some x, true ->
    x
  | _, Some _, false ->
    Code_error.raise "(formatting ...) stanza requires version 2.0" []
  | Some ext, _, true ->
    let suggestion =
      match to_explicit ext with
      | Some { enabled_for; _ } ->
        let dlang = encode_explicit enabled_for in
        [ Pp.textf "To port it to the new syntax, you can replace this part by:"
        ; Pp.tag User_message.Style.Details (Dune_lang.pp dlang)
        ]
      | None ->
        [ Pp.textf "To port it to the new syntax, you can delete this part." ]
    in
    User_error.raise ~loc:ext.loc
      (Pp.textf
         "Starting with (lang dune 2.0), formatting is enabled by default."
      :: suggestion)

let equal { enabled_for; _ } t = Enabled_for.equal enabled_for t.enabled_for
