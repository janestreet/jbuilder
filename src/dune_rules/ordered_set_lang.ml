open! Stdune
open! Dune_engine
open Import

module Ast = struct
  [@@@warning "-37"]

  type expanded = Expanded

  type unexpanded = Unexpanded

  type ('a, _) t =
    | Element : 'a -> ('a, _) t
    | Standard : ('a, _) t
    | Union : ('a, 'b) t list -> ('a, 'b) t
    | Diff : ('a, 'b) t * ('a, 'b) t -> ('a, 'b) t
    | Include : String_with_vars.t -> ('a, unexpanded) t

  let rec equal f x y =
    match (x, y) with
    | Element x, Element y -> f x y
    | Standard, Standard -> true
    | Union x, Union y -> List.equal (equal f) x y
    | Diff (x, x'), Diff (y, y') ->
      Tuple.T2.equal (equal f) (equal f) (x, x') (y, y')
    | _, _ -> false

  let union = function
    | [ x ] -> x
    | xs -> Union xs
end

type 'ast generic =
  { ast : 'ast
  ; loc : Loc.t option
  ; context : Univ_map.t (* Parsing context for Dune_lang.Decoder.parse *)
  }

let equal_generic f { ast; loc = _; context } t =
  f ast t.ast && context == t.context

type ast_expanded = (Loc.t * string, Ast.expanded) Ast.t

(* TODO this type should really be parameterized by the type of elements
   contained in the set, like we do with the predicate language. *)
type t = ast_expanded generic

let of_atoms ~loc lst =
  let ast = Ast.Union (List.map lst ~f:(fun s -> Ast.Element (loc, s))) in
  { ast; loc = Some loc; context = Univ_map.empty }

let equal = equal_generic (Ast.equal (fun (_, x) (_, y) -> String.equal x y))

let loc t = t.loc

module Parse = struct
  open Dune_lang.Decoder
  open Ast

  let generic ~inc ~elt =
    let open Dune_lang.Decoder in
    let rec one () =
      peek_exn >>= function
      | Atom (loc, A "\\") -> User_error.raise ~loc [ Pp.text "unexpected \\" ]
      | Atom (_, A "")
      | Quoted_string (_, _)
      | Template _ ->
        elt
      | Atom (loc, A s) -> (
        match s with
        | ":standard" -> junk >>> return Standard
        | ":include" ->
          User_error.raise ~loc
            [ Pp.text
                "Invalid use of :include, should be: (:include <filename>)"
            ]
        | _ when s.[0] = ':' ->
          User_error.raise ~loc [ Pp.textf "undefined symbol %s" s ]
        | _ -> elt)
      | List (_, Atom (loc, A s) :: _) -> (
        match s with
        | ":include" -> inc
        | s when s <> "" && s.[0] <> '-' && s.[0] <> ':' ->
          User_error.raise ~loc
            [ Pp.text
                "This atom must be quoted because it is the first element of a \
                 list and doesn't start with - or:"
            ]
        | _ -> enter (many []))
      | List _ -> enter (many [])
    and many acc =
      peek >>= function
      | None -> return (Union (List.rev acc))
      | Some (Atom (_, A "\\")) ->
        let+ to_remove = junk >>> many [] in
        Diff (Union (List.rev acc), to_remove)
      | Some _ ->
        let* x = one () in
        many (x :: acc)
    in
    many []

  let with_include ~elt =
    generic ~elt
      ~inc:
        (sum [ (":include", String_with_vars.decode >>| fun s -> Include s) ])

  let without_include ~elt =
    generic ~elt
      ~inc:
        (enter
           (let* loc = loc in
            User_error.raise ~loc
              [ Pp.text "(:include ...) is not allowed here" ]))
end

let decode =
  let open Dune_lang.Decoder in
  let+ context = get_all
  and+ loc, ast =
    located
      (Parse.without_include
         ~elt:(plain_string (fun ~loc s -> Ast.Element (loc, s))))
  in
  { ast; loc = Some loc; context }

let is_standard t =
  match (t.ast : ast_expanded) with
  | Ast.Standard -> true
  | _ -> false

module Eval = struct
  let of_ast ~diff ~singleton ~union t ~parse ~standard =
    let rec loop (t : ast_expanded) =
      match t with
      | Ast.Element (loc, s) ->
        let x = parse ~loc s in
        singleton x
      | Ast.Standard -> standard
      | Ast.Union elts -> union (List.map elts ~f:loop)
      | Ast.Diff (left, right) ->
        let left = loop left in
        let right = loop right in
        diff left right
    in
    if is_standard t then
      standard
    else
      loop t.ast

  let ordered eq =
    let singleton = List.singleton in
    let union = List.flatten in
    let diff a b =
      List.filter a ~f:(fun x -> List.for_all b ~f:(fun y -> not (eq x y)))
    in
    of_ast ~diff ~singleton ~union

  let unordered ~singleton ~empty ~merge ~key =
    let singleton x = singleton (key x) x in
    let union =
      List.fold_left ~init:empty ~f:(fun acc t ->
          merge acc t ~f:(fun _name x y ->
              match (x, y) with
              | Some x, _
              | _, Some x ->
                Some x
              | _ -> None))
    in
    let diff a b =
      merge a b ~f:(fun _name x y ->
          match (x, y) with
          | Some _, None -> x
          | _ -> None)
    in
    of_ast ~diff ~singleton ~union
end

let eval t ~parse ~eq ~standard = Eval.ordered eq t ~parse ~standard

module Unordered (Key : Ordered_set_lang_intf.Key) = struct
  type nonrec t = t

  module Key = Key

  let eval t ~parse ~key ~standard =
    let singleton = Key.Map.singleton in
    let empty = Key.Map.empty in
    let merge = Key.Map.merge in
    Eval.unordered ~singleton ~empty ~merge ~key t ~parse ~standard

  let loc_parse f ~loc s = (loc, f ~loc s)

  let eval_loc t ~parse ~key ~standard =
    eval t ~parse:(loc_parse parse) ~key:(fun (_, x) -> key x) ~standard
end

let eval_loc t ~parse ~eq ~standard =
  let loc_parse f ~loc s = (loc, f ~loc s) in
  let eq (_, a) (_, b) = eq a b in
  eval t ~parse:(loc_parse parse) ~standard ~eq

let standard = { ast = Ast.Standard; loc = None; context = Univ_map.empty }

let replace_standard ~where ~with_ : ast_expanded generic =
  let rec f (t : ast_expanded) : ast_expanded =
    match t with
    | Ast.Element x -> Element x
    | Ast.Standard -> with_
    | Ast.Union xs -> Union (List.map xs ~f)
    | Ast.Diff (x, y) -> Diff (f x, f y)
  in
  { ast = f where.ast; loc = where.loc; context = where.context }

let replace_standard_with_empty where =
  replace_standard ~where ~with_:(Union [] : ast_expanded)

let field ?check name =
  let decode =
    match check with
    | None -> decode
    | Some x -> Dune_lang.Decoder.( >>> ) x decode
  in
  Dune_lang.Decoder.field name decode ~default:standard

module Unexpanded = struct
  type ast = (String_with_vars.t, Ast.unexpanded) Ast.t

  type t = ast generic

  let equal x y = equal_generic (Ast.equal String_with_vars.equal_no_loc) x y

  let decode : t Dune_lang.Decoder.t =
    let open Dune_lang.Decoder in
    let+ context = get_all
    and+ loc, ast =
      located
        (Parse.with_include
           ~elt:(String_with_vars.decode >>| fun s -> Ast.Element s))
    in
    { ast; loc = Some loc; context }

  let encode t =
    let open Ast in
    let rec loop = function
      | Element s -> String_with_vars.encode s
      | Standard -> Dune_lang.atom ":standard"
      | Union l -> List (List.map l ~f:loop)
      | Diff (a, b) -> List [ loop a; Dune_lang.atom "\\"; loop b ]
      | Include fn ->
        List [ Dune_lang.atom ":include"; String_with_vars.encode fn ]
    in
    match t.ast with
    | Union l -> List.map l ~f:loop
    | Diff (a, b) -> [ loop a; Dune_lang.atom "\\"; loop b ]
    | ast -> [ loop ast ]

  let standard = standard

  let of_strings ~pos l =
    { ast =
        Ast.Union
          (List.map l ~f:(fun x ->
               Ast.Element (String_with_vars.virt_text pos x)))
    ; loc = Some (Loc.of_pos pos)
    ; context = Univ_map.empty
    }

  let include_single ~context ~pos f =
    { ast = Ast.Include (String_with_vars.virt_text pos f)
    ; loc = Some (Loc.of_pos pos)
    ; context
    }


  let field ?check name =
    let decode =
      match check with
      | None -> decode
      | Some x -> Dune_lang.Decoder.( >>> ) x decode
    in
    Dune_lang.Decoder.field name decode ~default:standard

  let has_special_forms t =
    let rec loop (t : ast) =
      let open Ast in
      match t with
      | Standard
      | Include _ ->
        true
      | Element _ -> false
      | Union l -> List.exists l ~f:loop
      | Diff (l, r) -> loop l || loop r
    in
    loop t.ast

  let has_standard t =
    let rec loop ast =
      match ast with
      | Ast.Standard -> true
      | Ast.Element _ -> false
      | Ast.Union l -> List.exists ~f:loop l
      | Ast.Diff (l, r) -> loop l || loop r
      | Ast.Include _ -> false
    in
    loop t.ast

  type position =
    | Pos
    | Neg

  let fold_strings t ~init ~f =
    let rec loop (t : ast) pos acc =
      let open Ast in
      match t with
      | Standard
      | Include _ ->
        acc
      | Element x -> f pos x acc
      | Union l -> List.fold_left l ~init:acc ~f:(fun acc x -> loop x pos acc)
      | Diff (l, r) ->
        let acc = loop l pos acc in
        let pos =
          match pos with
          | Pos -> Neg
          | Neg -> Pos
        in
        loop r pos acc
    in
    loop t.ast Pos init

  let expand t ~dir
      ~(f : Value.t list Action_builder.t String_with_vars.expander) =
    let open Action_builder.O in
    let context = t.context in
    let expand_template ~mode sw =
      Action_builder.Expander.expand sw ~mode ~dir ~f
    in
    let f_elems s =
      let loc = String_with_vars.loc s in
      let+ l = expand_template s ~mode:Many in
      Ast.union
        (List.map l ~f:(fun s -> Ast.Element (loc, Value.to_string ~dir s)))
    in
    let rec expand ~allow_include (t : ast) : ast_expanded Action_builder.t =
      let open Ast in
      match t with
      | Element s -> f_elems s
      | Standard -> Action_builder.return Standard
      | Include fn ->
        let loc = String_with_vars.loc fn in
        if not allow_include then
          User_error.raise ~loc [ Pp.text "(:include ...) is not allowed here" ]
        else
          let* sexp =
            let* path = expand_template fn ~mode:Single in
            let path = Value.to_path path ?error_loc:(Some loc) ~dir in
            Action_builder.read_sexp path
          in
          let t = Dune_lang.Decoder.parse decode context sexp in
          expand t.ast ~allow_include:false
      | Union l ->
        let+ l = Action_builder.all (List.map l ~f:(expand ~allow_include)) in
        Union l
      | Diff (l, r) ->
        let+ l = expand l ~allow_include
        and+ r = expand r ~allow_include in
        Diff (l, r)
    in
    let+ ast = expand t.ast ~allow_include:true in
    { t with ast }
end

module Unordered_string = Unordered (String)
