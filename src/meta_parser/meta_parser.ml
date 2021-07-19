(** Used inside Dune and in the outside library dune_site.plugin *)

module Make (Stdune : sig
  module Loc : sig
    type t

    val of_lexbuf : Lexing.lexbuf -> t
  end

  module Lib_name : sig
    type t

    val parse_string_exn : Loc.t * string -> t
  end

  module Pp : sig
    type +'tag t

    val text : string -> _ t
  end

  module User_message : sig
    module Style : sig
      type t
    end
  end

  module User_error : sig
    module Annot : sig
      type t
    end

    val raise :
         ?loc:Loc.t
      -> ?hints:User_message.Style.t Pp.t list
      -> ?annots:Annot.t list
      -> User_message.Style.t Pp.t list
      -> _
  end
end) =
struct
  open Stdune

  type t =
    { name : Lib_name.t option
    ; entries : entry list
    }

  and entry =
    | Comment of string
    | Rule of rule
    | Package of t

  and rule =
    { var : string
    ; predicates : predicate list
    ; action : action
    ; value : string
    }

  and action =
    | Set
    | Add

  and predicate =
    | Pos of string
    | Neg of string

  let add_versions t ~get_version =
    let rec map_entries ~rev_path ~has_version ~has_rules = function
      | [] -> (
        if has_version || not has_rules then
          []
        else
          match get_version (List.rev rev_path) with
          | None -> []
          | Some v ->
            [ Rule { var = "version"; predicates = []; action = Set; value = v }
            ])
      | entry :: entries -> (
        match entry with
        | Comment _ ->
          entry :: map_entries entries ~rev_path ~has_version ~has_rules
        | Rule rule ->
          entry
          :: map_entries entries ~rev_path
               ~has_version:(has_version || String.equal rule.var "version")
               ~has_rules:true
        | Package t ->
          Package (map_package t ~rev_path)
          :: map_entries entries ~rev_path ~has_version ~has_rules)
    and map_package t ~rev_path =
      let rev_path =
        match t.name with
        | None -> rev_path
        | Some n -> n :: rev_path
      in
      { t with
        entries =
          map_entries t.entries ~rev_path ~has_version:false ~has_rules:false
      }
    in
    map_package t ~rev_path:[]

  module Parse = struct
    let error lexbuf msg =
      User_error.raise ~loc:(Loc.of_lexbuf lexbuf) [ Pp.text msg ]

    let next =
      let user_error lexbuf msg =
        Stdune.User_error.raise
          ~loc:(Stdune.Loc.of_lexbuf lexbuf)
          [ Stdune.Pp.text msg ]
      in
      Meta_lexer.token { user_error }

    let package_name lb =
      match next lb with
      | String s ->
        if String.contains s '.' then
          error lb "'.' not allowed in sub-package names";
        let loc = Loc.of_lexbuf lb in
        Lib_name.parse_string_exn (loc, s)
      | _ -> error lb "package name expected"

    let string lb =
      match next lb with
      | String s -> s
      | _ -> error lb "string expected"

    let lparen lb =
      match next lb with
      | Lparen -> ()
      | _ -> error lb "'(' expected"

    let action lb =
      match next lb with
      | Equal -> Set
      | Plus_equal -> Add
      | _ -> error lb "'=' or '+=' expected"

    let rec predicates_and_action lb acc =
      match next lb with
      | Rparen -> (List.rev acc, action lb)
      | Name n -> after_predicate lb (Pos n :: acc)
      | Minus ->
        let n =
          match next lb with
          | Name p -> p
          | _ -> error lb "name expected"
        in
        after_predicate lb (Neg n :: acc)
      | _ -> error lb "name, '-' or ')' expected"

    and after_predicate lb acc =
      match next lb with
      | Rparen -> (List.rev acc, action lb)
      | Comma -> predicates_and_action lb acc
      | _ -> error lb "')' or ',' expected"

    let rec entries lb depth acc =
      match next lb with
      | Rparen ->
        if depth > 0 then
          List.rev acc
        else
          error lb "closing parenthesis without matching opening one"
      | Eof ->
        if depth = 0 then
          List.rev acc
        else
          error lb (Printf.sprintf "%d closing parentheses missing" depth)
      | Name "package" ->
        let name = package_name lb in
        lparen lb;
        let sub_entries = entries lb (depth + 1) [] in
        entries lb depth
          (Package { name = Some name; entries = sub_entries } :: acc)
      | Name var ->
        let predicates, action =
          match next lb with
          | Equal -> ([], Set)
          | Plus_equal -> ([], Add)
          | Lparen -> predicates_and_action lb []
          | _ -> error lb "'=', '+=' or '(' expected"
        in
        let value = string lb in
        entries lb depth (Rule { var; predicates; action; value } :: acc)
      | _ -> error lb "'package' or variable name expected"
  end
end
