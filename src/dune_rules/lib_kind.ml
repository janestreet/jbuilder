open! Dune_engine
open Stdune

module Ppx_args = struct
  module Cookie = struct
    type t =
      { name : string
      ; value : String_with_vars.t
      }

    let to_dyn x =
      let open Dyn.Encoder in
      record
        [ ("name", string x.name); ("value", String_with_vars.to_dyn x.value) ]

    let decode =
      let open Dune_lang.Decoder in
      let* () = Dune_lang.Syntax.since Stanza.syntax (1, 10) in
      enter
        (let+ name =
           plain_string (fun ~loc str ->
               if String.contains str '=' then
                 User_error.raise ~loc
                   [ Pp.text "Character '=' is not allowed in cookie names" ]
               else
                 str)
         and+ value = String_with_vars.decode in
         { name; value })

    let encode { name; value } =
      let open Dune_lang in
      List [ Encoder.string name; String_with_vars.encode value ]
  end

  type t = { cookies : Cookie.t list }

  let empty = { cookies = [] }

  let to_dyn { cookies } =
    let open Dyn.Encoder in
    record [ ("cookies", list Cookie.to_dyn cookies) ]

  let decode =
    let open Dune_lang.Decoder in
    let args =
      let+ cookies = field "cookies" (repeat Cookie.decode) ~default:[] in
      { cookies }
    in
    fields args

  let encode { cookies } =
    let open Dune_lang.Encoder in
    record_fields [ field_l "cookies" Cookie.encode cookies ]
end

type t =
  | Normal
  | Ppx_deriver of Ppx_args.t
  | Ppx_rewriter of Ppx_args.t

let to_dyn x =
  let open Dyn.Encoder in
  match x with
  | Normal -> constr "Normal" []
  | Ppx_deriver args -> constr "Ppx_deriver" [ Ppx_args.to_dyn args ]
  | Ppx_rewriter args -> constr "Ppx_rewriter" [ Ppx_args.to_dyn args ]

let decode =
  let open Dune_lang.Decoder in
  sum
    [ ("normal", return Normal)
    ; ( "ppx_deriver"
      , let+ args = Ppx_args.decode in
        Ppx_deriver args )
    ; ( "ppx_rewriter"
      , let+ args = Ppx_args.decode in
        Ppx_rewriter args )
    ]

let encode t =
  match
    match t with
    | Normal -> Dune_lang.atom "normal"
    | Ppx_deriver x -> List (Dune_lang.atom "ppx_deriver" :: Ppx_args.encode x)
    | Ppx_rewriter x -> List (Dune_lang.atom "ppx_rewriter" :: Ppx_args.encode x)
  with
  | List [ x ] -> x
  | x -> x
