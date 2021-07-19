module List = ListLabels
module String = StringLabels

module Json = struct
  type t =
    [ `Int of int
    | `Float of float
    | `String of string
    | `List of t list
    | `Bool of bool
    | `Assoc of (string * t) list
    ]
end

module Timestamp : sig
  type t

  val to_json : t -> Json.t

  val now : unit -> t

  val of_float_seconds : float -> t
end = struct
  type t = float

  let now () = Unix.gettimeofday ()

  let of_float_seconds x = x

  let to_json f =
    let n = int_of_float @@ (f *. 1_000_000.) in
    `Int n
end

module Event = struct
  [@@@ocaml.warning "-37"]

  module Timestamp = Timestamp

  type common_fields =
    { name : string
    ; cat : string list
    ; ts : Timestamp.t
    ; tts : Timestamp.t option
    ; pid : int
    ; tid : int
    ; cname : string option
    }

  let common_fields ?tts ?cname ?(cat = []) ?(pid = 0) ?(tid = 0) ~ts ~name () =
    { tts; cname; cat; ts; pid; tid; name }

  let set_ts t ts = { t with ts }

  type scope =
    | Global
    | Process
    | Thread

  type async =
    | Start
    | Instant
    | End

  type args = (string * Json.t) list

  module Id = struct
    type t =
      | Int of int
      | String of string

    let to_json = function
      | Int i -> `Int i
      | String s -> `String s

    let field id = ("id", to_json id)
  end

  type object_kind =
    | New
    | Snapshot of
        { cat : string list option
        ; args : args
        }
    | Destroy

  type metadata =
    | Process_name of
        { pid : int
        ; name : string
        }
    | Process_labels of
        { pid : int
        ; labels : string
        }
    | Thread_name of
        { tid : int
        ; pid : int
        ; name : string
        }
    | Process_sort_index of
        { pid : int
        ; sort_index : int
        }
    | Thread_sort_index of
        { pid : int
        ; tid : int
        ; sort_index : int
        }

  (* TODO support flow, samples, referemces, memory dumps *)
  type t =
    | Counter of common_fields * args * Id.t option
    | Duration_start of common_fields * args * Id.t option
    | Duration_end of
        { pid : int
        ; tid : int
        ; ts : float
        ; args : args option
        }
    | Complete of
        { common : common_fields
        ; args : args option
        ; dur : Timestamp.t
        ; tdur : Timestamp.t option
        }
    | Instant of common_fields * scope option * args option
    | Async of
        { common : common_fields
        ; async : async
        ; scope : string option
        ; id : Id.t
        ; args : args option
        }
    | Object of
        { common : common_fields
        ; object_kind : object_kind
        ; id : Id.t
        ; scope : string option
        }
    | Metadata of metadata

  let phase s = ("ph", `String s)

  let add_field_opt to_field field fields =
    match field with
    | None -> fields
    | Some f -> to_field f :: fields

  let json_fields_of_common_fields { name; cat; ts; tts; pid; tid; cname } =
    let fields =
      [ ("name", `String name)
      ; ("cat", `String (String.concat ~sep:"," cat))
      ; ("ts", Timestamp.to_json ts)
      ; ("pid", `Int pid)
      ; ("tid", `Int tid)
      ]
    in
    let fields =
      add_field_opt (fun cname -> ("cname", `String cname)) cname fields
    in
    add_field_opt (fun tts -> ("tts", Timestamp.to_json tts)) tts fields

  let json_of_scope = function
    | Global -> `String "g"
    | Process -> `String "p"
    | Thread -> `String "t"

  let args_field fields = ("args", `Assoc fields)

  let json_fields_of_metadata m =
    let fields =
      let common pid name = [ ("name", `String name); ("pid", `Int pid) ] in
      match m with
      | Process_name { pid; name } ->
        args_field [ ("name", `String name) ] :: common pid "thread_name"
      | Process_labels { pid; labels } ->
        args_field [ ("labels", `String labels) ] :: common pid "process_labels"
      | Thread_name { tid; pid; name } ->
        ("tid", `Int tid)
        :: args_field [ ("name", `String name) ]
        :: common pid "process_name"
      | Process_sort_index { pid; sort_index } ->
        args_field [ ("sort_index", `Int sort_index) ]
        :: common pid "process_sort_index"
      | Thread_sort_index { pid; sort_index; tid } ->
        ("tid", `Int tid)
        :: args_field [ ("sort_index", `Int sort_index) ]
        :: common pid "thread_sort_index"
    in
    phase "M" :: fields

  let to_json_fields : t -> (string * Json.t) list = function
    | Counter (common, args, id) ->
      let fields = json_fields_of_common_fields common in
      let fields = phase "C" :: args_field args :: fields in
      add_field_opt Id.field id fields
    | Duration_start (common, args, id) ->
      let fields = json_fields_of_common_fields common in
      let fields = phase "B" :: args_field args :: fields in
      add_field_opt Id.field id fields
    | Duration_end { pid; tid; ts; args } ->
      let fields =
        [ ("tid", `Int tid); ("pid", `Int pid); ("ts", `Float ts); phase "E" ]
      in
      add_field_opt args_field args fields
    | Complete { common; dur; args; tdur } ->
      let fields = json_fields_of_common_fields common in
      let fields = phase "X" :: ("dur", Timestamp.to_json dur) :: fields in
      let fields =
        add_field_opt (fun tdur -> ("tdur", Timestamp.to_json tdur)) tdur fields
      in
      add_field_opt args_field args fields
    | Instant (common, scope, args) ->
      let fields = json_fields_of_common_fields common in
      let fields = phase "i" :: fields in
      let fields =
        add_field_opt (fun s -> ("s", json_of_scope s)) scope fields
      in
      add_field_opt args_field args fields
    | Async { common; async; scope; id; args } ->
      let fields = json_fields_of_common_fields common in
      let fields = Id.field id :: fields in
      let fields =
        let ph =
          let s =
            match async with
            | Start -> "b"
            | Instant -> "n"
            | End -> "e"
          in
          phase s
        in
        ph :: fields
      in
      let fields = add_field_opt (fun s -> ("scope", `String s)) scope fields in
      add_field_opt args_field args fields
    | Object { common; object_kind; id; scope } ->
      let fields = json_fields_of_common_fields common in
      let fields = Id.field id :: fields in
      let fields =
        let ph, args =
          match object_kind with
          | New -> ("N", None)
          | Destroy -> ("D", None)
          | Snapshot { cat; args } ->
            let snapshot =
              add_field_opt
                (fun cat -> ("cat", `String (String.concat ~sep:"," cat)))
                cat args
            in
            ("O", Some [ ("snapshot", `Assoc snapshot) ])
        in
        let fields = phase ph :: fields in
        add_field_opt args_field args fields
      in
      add_field_opt (fun s -> ("scope", `String s)) scope fields
    | Metadata m -> json_fields_of_metadata m

  let to_json t = `Assoc (to_json_fields t)

  let counter ?id common args = Counter (common, args, id)

  let complete ?tdur ?args ~dur common = Complete { common; tdur; dur; args }

  let async ?scope ?args id async common =
    Async { common; args; scope; id; async }
end
