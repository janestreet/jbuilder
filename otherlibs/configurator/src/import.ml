let sprintf = Printf.sprintf

let eprintf = Printf.eprintf

let ( ^/ ) = Filename.concat

exception Fatal_error of string

let die fmt = Printf.ksprintf (fun s -> raise (Fatal_error s)) fmt

let warn fmt =
  Printf.ksprintf (fun msg -> prerr_endline ("Warning: " ^ msg)) fmt

module Result = struct
  type ('a, 'b) t = ('a, 'b) result =
    | Ok of 'a
    | Error of 'b

  let to_option = function
    | Ok x -> Some x
    | Error _ -> None
end

module Exn = struct
  external reraise : exn -> _ = "%reraise"

  let protectx x ~f ~finally =
    match f x with
    | y ->
      finally x;
      y
    | exception e ->
      finally x;
      raise e

  let protect ~f ~finally = protectx () ~f ~finally

  include struct
    [@@@ocaml.warning "-32"]

    let raise_with_backtrace exn _bt = reraise exn
  end

  include Printexc
end

module Option = struct
  let map t ~f =
    match t with
    | None -> None
    | Some x -> Some (f x)

  let some_if cond x =
    if cond then
      Some x
    else
      None

  let some x = Some x

  let try_with f =
    match f () with
    | exception _ -> None
    | x -> Some x

  let iter t ~f =
    match t with
    | None -> ()
    | Some x -> f x

  module O = struct
    let ( >>= ) x f =
      match x with
      | None -> None
      | Some x -> f x

    let ( >>| ) x f = map x ~f
  end
end

module List = struct
  include ListLabels

  let rec find_map l ~f =
    match l with
    | [] -> None
    | x :: l -> (
      match f x with
      | None -> find_map l ~f
      | Some _ as res -> res)
end

module Array = ArrayLabels

module Bool = struct
  let of_string s =
    match bool_of_string s with
    | s -> Some s
    | exception _ -> None
end

module Map (S : Map.OrderedType) = struct
  module M = MoreLabels.Map.Make (S)
  include M

  let update (type a) (t : a t) (key : M.key) ~(f : a option -> a option) : a t
      =
    let v =
      match find key t with
      | exception Not_found -> None
      | v -> Some v
    in
    match (f v, v) with
    | None, None -> t
    | None, Some _ -> remove key t
    | Some data, _ -> add ~key ~data t

  let find m k =
    match find k m with
    | exception Not_found -> None
    | s -> Some s

  let set t k v = add ~key:k ~data:v t

  let of_list =
    let rec loop acc = function
      | [] -> Result.Ok acc
      | (k, v) :: l -> (
        match find acc k with
        | None -> loop (set acc k v) l
        | Some v_old -> Error (k, v_old, v))
    in
    fun l -> loop empty l

  let of_list_exn l =
    match of_list l with
    | Ok s -> s
    | Error (_, _, _) -> failwith "Map.of_list_exn: duplicate key"
end

module Int = struct
  let of_string s =
    match int_of_string s with
    | s -> Some s
    | exception _ -> None

  module Map = struct
    include Map (struct
      type t = int

      let compare = compare
    end)
  end
end

module Bytes = struct
  include struct
    [@@@ocaml.warning "-32"]

    let blit_string ~(src : string) ~src_pos ~(dst : Bytes.t) ~dst_pos ~len =
      for i = 0 to len - 1 do
        Bytes.set dst (i + dst_pos) src.[i + src_pos]
      done
  end

  include BytesLabels
end

module String = struct
  include StringLabels
  module Map = Map (String)

  let take s i = sub s ~pos:0 ~len:(min i (String.length s))

  let drop s n =
    let len = length s in
    sub s ~pos:(min n len) ~len:(max (len - n) 0)

  let index s i =
    match String.index s i with
    | exception Not_found -> None
    | s -> Some s

  let split_lines s =
    let rec loop ~last_is_cr ~acc i j =
      if j = length s then
        let acc =
          if j = i || (j = i + 1 && last_is_cr) then
            acc
          else
            sub s ~pos:i ~len:(j - i) :: acc
        in
        List.rev acc
      else
        match s.[j] with
        | '\r' -> loop ~last_is_cr:true ~acc i (j + 1)
        | '\n' ->
          let line =
            let len =
              if last_is_cr then
                j - i - 1
              else
                j - i
            in
            sub s ~pos:i ~len
          in
          loop ~acc:(line :: acc) (j + 1) (j + 1) ~last_is_cr:false
        | _ -> loop ~acc i (j + 1) ~last_is_cr:false
    in
    loop ~acc:[] 0 0 ~last_is_cr:false

  let exists =
    let rec loop s i len f =
      if i = len then
        false
      else
        f (unsafe_get s i) || loop s (i + 1) len f
    in
    fun s ~f -> loop s 0 (length s) f

  let is_empty = function
    | "" -> true
    | _ -> false

  let extract_words s ~is_word_char =
    let rec skip_blanks i =
      if i = length s then
        []
      else if is_word_char s.[i] then
        parse_word i (i + 1)
      else
        skip_blanks (i + 1)
    and parse_word i j =
      if j = length s then
        [ sub s ~pos:i ~len:(j - i) ]
      else if is_word_char s.[j] then
        parse_word i (j + 1)
      else
        sub s ~pos:i ~len:(j - i) :: skip_blanks (j + 1)
    in
    skip_blanks 0

  let extract_comma_space_separated_words s =
    extract_words s ~is_word_char:(function
      | ','
      | ' '
      | '\t'
      | '\n' ->
        false
      | _ -> true)

  let extract_blank_separated_words s =
    extract_words s ~is_word_char:(function
      | ' '
      | '\t' ->
        false
      | _ -> true)

  let split s ~on =
    let rec loop i j =
      if j = length s then
        [ sub s ~pos:i ~len:(j - i) ]
      else if s.[j] = on then
        sub s ~pos:i ~len:(j - i) :: loop (j + 1) (j + 1)
      else
        loop i (j + 1)
    in
    loop 0 0
end

module Io = struct
  let open_in ?(binary = true) fn =
    if binary then
      open_in_bin fn
    else
      open_in fn

  let open_out ?(binary = true) fn =
    if binary then
      open_out_bin fn
    else
      open_out fn

  let input_lines =
    let rec loop ic acc =
      match input_line ic with
      | exception End_of_file -> List.rev acc
      | line -> loop ic (line :: acc)
    in
    fun ic -> loop ic []

  let with_file_in ?binary fn ~f =
    Exn.protectx (open_in ?binary fn) ~finally:close_in ~f

  let with_file_out ?binary p ~f =
    Exn.protectx (open_out ?binary p) ~finally:close_out ~f

  let write_file ?binary fn data =
    with_file_out ?binary fn ~f:(fun oc -> output_string oc data)

  let write_lines ?binary fn lines =
    with_file_out ?binary fn ~f:(fun oc ->
        List.iter
          ~f:(fun line ->
            output_string oc line;
            output_string oc "\n")
          lines)

  let read_all =
    (* We use 65536 because that is the size of OCaml's IO buffers. *)
    let chunk_size = 65536 in
    (* Generic function for channels such that seeking is unsupported or
       broken *)
    let read_all_generic t buffer =
      let rec loop () =
        Buffer.add_channel buffer t chunk_size;
        loop ()
      in
      try loop () with
      | End_of_file -> Buffer.contents buffer
    in
    fun t ->
      (* Optimisation for regular files: if the channel supports seeking, we
         compute the length of the file so that we read exactly what we need and
         avoid an extra memory copy. We expect that most files Dune reads are
         regular files so this optimizations seems worth it. *)
      match in_channel_length t with
      | exception _ -> read_all_generic t (Buffer.create chunk_size)
      | n -> (
        let s = really_input_string t n in
        (* For some files [in_channel_length] returns an invalid value. For
           instance for files in /proc it returns [0]. So we try to read one
           more character to make sure we did indeed reach the end of the
           file *)
        match input_char t with
        | exception End_of_file -> s
        | c ->
          (* The [+ chunk_size] is to make sure there is at least [chunk_size]
             free space so that the first [Buffer.add_channel buffer t
             chunk_size] in [read_all_generic] does not grow the buffer. *)
          let buffer = Buffer.create (String.length s + 1 + chunk_size) in
          Buffer.add_string buffer s;
          Buffer.add_char buffer c;
          read_all_generic t buffer)

  let read_file ?binary fn = with_file_in fn ~f:read_all ?binary

  let with_lexbuf_from_file fn ~f =
    with_file_in fn ~f:(fun ic ->
        let lb = Lexing.from_channel ic in
        lb.lex_curr_p <-
          { pos_fname = fn; pos_lnum = 1; pos_bol = 0; pos_cnum = 0 };
        f lb)
end

module Sexp = struct
  module T = struct
    type t =
      | Atom of string
      | List of t list
  end

  include T
  include Csexp.Make (T)
end
