include Dyn0

let rec pp = function
  | Unit -> Pp.string "()"
  | Int i -> Pp.int i
  | Bool b -> Pp.bool b
  | String s -> Pp.string s
  | Bytes b -> Pp.string (Bytes.to_string b)
  | Char c -> Pp.char c
  | Float f -> Pp.float f
  | Option None -> pp (Variant ("None", []))
  | Option (Some x) -> pp (Variant ("Some", [x]))
  | List x ->
    Pp.box
      [ Pp.char '['
      ; Pp.list ~sep:(Pp.seq (Pp.char ';') Pp.space) x ~f:pp
      ; Pp.char ']'
      ]
  | Array a ->
    Pp.box
      [ Pp.string "[|"
      ; Pp.list ~sep:(Pp.seq (Pp.char ';') Pp.space) (Array.to_list a) ~f:pp
      ; Pp.string "|]"
      ]
  | Set xs ->
    Pp.box
      [ Pp.string "set {"
      ; Pp.list ~sep:(Pp.seq (Pp.char ';') Pp.space) xs ~f:pp
      ; Pp.string "}"
      ]
  | Map xs ->
    Pp.box
      [ Pp.string "map {"
      ; Pp.list ~sep:(Pp.seq (Pp.char ';') Pp.space) xs ~f:(fun (k, v) ->
          Pp.box
            [ pp k
            ; Pp.space
            ; Pp.string ":"
            ; Pp.space
            ; pp v
            ]
        )
      ; Pp.string "}"
      ]
  | Tuple x ->
    Pp.box
      [ Pp.char '('
      ; Pp.list ~sep:(Pp.seq (Pp.char ',') Pp.space) x ~f:pp
      ; Pp.char ')'
      ]
  | Record fields ->
    Pp.vbox ~indent:2
      [ Pp.char '{'
      ; Pp.list ~sep:(Pp.char ';') fields ~f:(fun (f, v) ->
          Pp.concat
            [ Pp.string f
            ; Pp.space
            ; Pp.char '='
            ; Pp.space
            ; Pp.box ~indent:2 [pp v]
            ]
        )
      ; Pp.char '}'
      ]
  | Variant (v, []) -> Pp.string v
  | Variant (v, xs) ->
    Pp.hvbox ~indent:2
      [ Pp.string v
      ; Pp.space
      ; Pp.list ~sep:(Pp.char ',') xs ~f:pp
      ]

let pp fmt t = Pp.pp fmt (pp t)

let rec to_sexp =
  let open Sexp.Encoder in
  function
  | Unit -> unit ()
  | Int i -> int i
  | Bool b -> bool b
  | String s -> string s
  | Bytes s -> string (Bytes.to_string s)
  | Char c -> char c
  | Float f -> float f
  | Option o -> option to_sexp o
  | List l -> list to_sexp l
  | Array a -> array to_sexp a
  | Map xs -> list (pair to_sexp to_sexp) xs
  | Set xs -> list to_sexp xs
  | Tuple t -> list to_sexp t
  | Record fields ->
    List.map fields ~f:(fun (field, f) -> (field, to_sexp f))
    |> record
  | Variant (s, []) -> string s
  | Variant (s, xs) -> constr s (List.map xs ~f:to_sexp)

let option f t =
  Option (
    match t with
    | None -> None
    | Some s -> Some (f s)
  )

let opaque = String "<opaque>"
