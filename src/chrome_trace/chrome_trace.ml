open Stdune

module Json = struct
  type t =
    | Int of int
    | Float of float
    | String of string
    | Array of t list
    | Bool of bool
    | Object of (string * t) list

  let quote_string_to_buf s buf =
    (* TODO: escaping is wrong here, in particular for control characters *)
    Buffer.add_string buf (sprintf "%S" s)

  let rec to_buf t buf =
    match t with
    | String s -> quote_string_to_buf s buf
    | Int i -> Buffer.add_string buf (string_of_int i)
    | Float f -> Buffer.add_string buf (string_of_float f)
    | Bool b -> Buffer.add_string buf (string_of_bool b)
    | Array l ->
      Buffer.add_char buf '[';
      array_body_to_buf l buf;
      Buffer.add_char buf ']'
    | Object o ->
      Buffer.add_char buf '{';
      object_body_to_buf o buf;
      Buffer.add_char buf '}'

  and array_body_to_buf t buf =
    match t with
    | [] -> ()
    | [ x ] -> to_buf x buf
    | x :: xs ->
      to_buf x buf;
      Buffer.add_char buf ',';
      array_body_to_buf xs buf

  and object_body_to_buf t buf =
    match t with
    | [] -> ()
    | [ (x, y) ] ->
      quote_string_to_buf x buf;
      Buffer.add_char buf ':';
      to_buf y buf
    | (x, y) :: xs ->
      quote_string_to_buf x buf;
      Buffer.add_char buf ':';
      to_buf y buf;
      Buffer.add_char buf ',';
      object_body_to_buf xs buf

  let to_string t =
    let buf = Buffer.create 0 in
    to_buf t buf;
    Buffer.contents buf
end

module Timestamp : sig
  type t

  val to_json : t -> Json.t

  val of_float : float -> t
end = struct
  type t = float

  let of_float x = x

  let to_json f =
    let n = int_of_float @@ (f *. 1_000_000.) in
    Json.Int n
end

type t =
  { print : string -> unit
  ; close : unit -> unit
  ; get_time : unit -> Timestamp.t
  ; gc_stat : unit -> Gc.stat
  ; buffer : Buffer.t
  ; mutable after_first_event : bool
  ; mutable next_id : int
  }

let fake_gc_stat =
  let init_gc = Gc.quick_stat () in
  { init_gc with
    Gc.minor_words = 0.
  ; promoted_words = 0.
  ; major_words = 0.
  ; minor_collections = 0
  ; major_collections = 0
  ; heap_words = 0
  ; heap_chunks = 0
  ; live_words = 0
  ; live_blocks = 0
  ; free_words = 0
  ; free_blocks = 0
  ; largest_free = 0
  ; fragments = 0
  ; compactions = 0
  ; top_heap_words = 0
  ; stack_size = 0
  }
  [@ocaml.warning "-23"]

(* all fields of record used *)

let fake time_ref buf =
  let print s = Buffer.add_string buf s in
  let close () = () in
  let get_time () = Timestamp.of_float !time_ref in
  let gc_stat () = fake_gc_stat in
  let buffer = Buffer.create 1024 in
  { print
  ; close
  ; get_time
  ; gc_stat
  ; after_first_event = false
  ; next_id = 0
  ; buffer
  }

let close { print; close; _ } =
  print "]\n";
  close ()

let make path =
  let channel = Stdlib.open_out path in
  let print s = Stdlib.output_string channel s in
  let close () = Stdlib.close_out channel in
  let get_time () = Timestamp.of_float (Unix.gettimeofday ()) in
  let gc_stat () = Gc.stat () in
  let buffer = Buffer.create 1024 in
  { print
  ; close
  ; get_time
  ; gc_stat
  ; after_first_event = false
  ; next_id = 0
  ; buffer
  }

let next_leading_char t =
  match t.after_first_event with
  | true -> ','
  | false ->
    t.after_first_event <- true;
    '['

let printf t format_string =
  let c = next_leading_char t in
  Printf.ksprintf t.print ("%c" ^^ format_string ^^ "\n") c

module Event = struct
  [@@@ocaml.warning "-37"]

  type common =
    { name : string
    ; cat : string list
    ; ts : Timestamp.t
    ; tts : Timestamp.t option
    ; pid : int
    ; tid : int
    ; cname : string option
    }

  let common ?tts ?cname ?(cat = []) ~ts ~name ~pid ~tid () =
    { tts; cname; cat; ts; pid; tid; name }

  type scope =
    | Global
    | Process
    | Thread

  type async_kind =
    | Start
    | Instant
    | End

  type args = (string * Json.t) list

  type id =
    | Int of int
    | String of string

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
    | Counter of common * args * id option
    | Duration_start of common * args * id option
    | Duration_end of
        { pid : int
        ; tid : int
        ; ts : float
        ; args : args option
        }
    | Complete of
        { common : common
        ; args : args option
        ; dur : Timestamp.t
        ; tdur : Timestamp.t option
        }
    | Instant of common * scope option * args option
    | Async of
        { common : common
        ; async_kind : async_kind
        ; scope : string option
        ; id : id
        ; args : args option
        }
    | Object of
        { common : common
        ; object_kind : object_kind
        ; id : id
        ; scope : string option
        }
    | Metadata of metadata

  let common_fields { name; cat; ts; tts; pid; tid; cname } =
    let fields =
      [ ("name", Json.String name)
      ; ("cat", String (String.concat ~sep:"," cat))
      ; ("ts", Timestamp.to_json ts)
      ; ("pid", Int pid)
      ; ("tid", Int tid)
      ]
    in
    let fields =
      match cname with
      | None -> fields
      | Some cname -> ("cname", String cname) :: fields
    in
    match tts with
    | None -> fields
    | Some tts -> ("tts", Timestamp.to_json tts) :: fields

  let json_of_id = function
    | Int i -> Json.Int i
    | String s -> Json.String s

  let json_of_scope = function
    | Global -> Json.String "g"
    | Process -> Json.String "p"
    | Thread -> Json.String "t"

  let json_fields_of_metadata m =
    let fields =
      match m with
      | Process_name { pid; name } ->
        [ ("name", Json.String "thread_name")
        ; ("pid", Int pid)
        ; ("args", Json.Object [ ("name", Json.String name) ])
        ]
      | Process_labels { pid; labels } ->
        [ ("name", Json.String "process_labels")
        ; ("pid", Int pid)
        ; ("args", Json.Object [ ("labels", Json.String labels) ])
        ]
      | Thread_name { tid; pid; name } ->
        [ ("name", Json.String "process_name")
        ; ("pid", Int pid)
        ; ("tid", Int tid)
        ; ("args", Json.Object [ ("name", Json.String name) ])
        ]
      | Process_sort_index { pid; sort_index } ->
        [ ("name", Json.String "process_sort_index")
        ; ("pid", Int pid)
        ; ("args", Json.Object [ ("sort_index", Json.Int sort_index) ])
        ]
      | Thread_sort_index { pid; sort_index; tid } ->
        [ ("name", Json.String "thread_sort_index")
        ; ("pid", Int pid)
        ; ("tid", Int tid)
        ; ("args", Json.Object [ ("sort_index", Json.Int sort_index) ])
        ]
    in
    ("ph", Json.String "M") :: fields

  let to_json_fields : t -> (string * Json.t) list = function
    | Counter (common, args, id) -> (
      let fields = common_fields common in
      let fields =
        ("ph", Json.String "C") :: ("args", Json.Object args) :: fields
      in
      match id with
      | None -> fields
      | Some id -> ("id", json_of_id id) :: fields )
    | Duration_start (common, args, id) -> (
      let fields = common_fields common in
      let fields =
        ("ph", Json.String "B") :: ("args", Json.Object args) :: fields
      in
      match id with
      | None -> fields
      | Some id -> ("id", json_of_id id) :: fields )
    | Duration_end { pid; tid; ts; args } -> (
      let fields =
        [ ("tid", Json.Int tid)
        ; ("pid", Int pid)
        ; ("ts", Json.Float ts)
        ; ("ph", String "E")
        ]
      in
      match args with
      | None -> fields
      | Some args -> ("args", Json.Object args) :: fields )
    | Complete { common; dur; args; tdur } -> (
      let fields = common_fields common in
      let fields =
        ("ph", Json.String "X") :: ("dur", Timestamp.to_json dur) :: fields
      in
      let fields =
        match tdur with
        | None -> fields
        | Some tdur -> ("tdur", Timestamp.to_json tdur) :: fields
      in
      match args with
      | None -> fields
      | Some args -> ("args", Json.Object args) :: fields )
    | Instant (common, scope, args) -> (
      let fields = common_fields common in
      let fields = ("ph", Json.String "i") :: fields in
      let fields =
        match scope with
        | None -> fields
        | Some s -> ("s", json_of_scope s) :: fields
      in
      match args with
      | None -> fields
      | Some args -> ("args", Json.Object args) :: fields )
    | Async { common; async_kind; scope; id; args } -> (
      let fields = common_fields common in
      let fields = ("id", json_of_id id) :: fields in
      let fields =
        let ph =
          let s =
            match async_kind with
            | Start -> "b"
            | Instant -> "n"
            | End -> "e"
          in
          ("ph", Json.String s)
        in
        ph :: fields
      in
      let fields =
        match scope with
        | None -> fields
        | Some s -> ("scope", Json.String s) :: fields
      in
      match args with
      | None -> fields
      | Some args -> ("args", Json.Object args) :: fields )
    | Object { common; object_kind; id; scope } -> (
      let fields = common_fields common in
      let fields = ("id", json_of_id id) :: fields in
      let fields =
        let ph, args =
          match object_kind with
          | New -> ("N", None)
          | Destroy -> ("D", None)
          | Snapshot { cat; args } ->
            let args =
              match cat with
              | None -> args
              | Some cat ->
                ("cat", Json.String (String.concat ~sep:"," cat)) :: args
            in
            ("O", Some (Json.Object [ ("snapshot", Json.Object args) ]))
        in
        let fields = ("ph", Json.String ph) :: fields in
        match args with
        | None -> fields
        | Some args -> ("args", args) :: fields
      in
      match scope with
      | None -> fields
      | Some s -> ("scope", Json.String s) :: fields )
    | Metadata m -> json_fields_of_metadata m

  let to_json t = Json.Object (to_json_fields t)
end

type event = Event.id * string

let emit_counter t key values =
  let time = t.get_time () in
  let event =
    let common = Event.common ~name:key ~pid:0 ~tid:0 ~ts:time () in
    Event.Counter (common, values, None)
  in
  printf t "%s" (Json.to_string (Event.to_json event))

let emit_gc_counters t =
  let stat = t.gc_stat () in
  emit_counter t "gc"
    [ ("live_words", Json.Int stat.live_words)
    ; ("free_words", Int stat.free_words)
    ; ("stack_size", Int stat.stack_size)
    ; ("heap_words", Int stat.heap_words)
    ; ("top_heap_words", Int stat.top_heap_words)
    ; ("minor_words", Float stat.minor_words)
    ; ("major_words", Float stat.major_words)
    ; ("promoted_words", Float stat.promoted_words)
    ; ("compactions", Int stat.compactions)
    ; ("major_collections", Int stat.major_collections)
    ; ("minor_collections", Int stat.minor_collections)
    ]

let next_id t =
  let r = t.next_id in
  t.next_id <- r + 1;
  Event.Int r

let on_process_start t ~program ~args =
  let name = Filename.basename program in
  let id = next_id t in
  let time = t.get_time () in
  let event =
    let common =
      Event.common ~cat:[ "process" ] ~name ~pid:0 ~tid:0 ~ts:time ()
    in
    let args =
      [ ( "process_args"
        , Json.Array (List.map args ~f:(fun arg -> Json.String arg)) )
      ]
    in
    Event.Async
      { common; async_kind = Start; scope = None; id; args = Some args }
  in
  printf t "%s" (Json.to_string (Event.to_json event));
  (id, name)

let on_process_end t (id, name) =
  let time = t.get_time () in
  let event =
    let common =
      Event.common ~cat:[ "process" ] ~name ~pid:0 ~tid:0 ~ts:time ()
    in
    Event.Async { common; async_kind = Start; scope = None; id; args = None }
  in
  printf t "%s" (Json.to_string (Event.to_json event))