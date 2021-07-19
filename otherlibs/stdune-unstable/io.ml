let close_in = close_in

let close_out = close_out

let close_both (ic, oc) =
  match close_out oc with
  | () -> close_in ic
  | exception exn ->
    close_in ic;
    Exn.reraise exn

let input_lines =
  let rec loop ic acc =
    match input_line ic with
    | exception End_of_file -> List.rev acc
    | line -> loop ic (line :: acc)
  in
  fun ic -> loop ic []

let input_zero_from_buffer from buf =
  match String.index_from_opt buf from '\x00' with
  | None -> None
  | Some eos -> Some (String.sub buf ~pos:from ~len:(eos - from), eos + 1)

(* Note, the complexity of this function will be bad if the zero-separated
   elements are much larger than the current input buffer *)
let input_zero_separated =
  (* Take all the \0-terminated strings from [buf], return the scanned list and
     the remainder *)
  let rec scan_inputs_buf from buf acc =
    (* note that from is untouched if input_zero_from_buffer returns None *)
    match input_zero_from_buffer from buf with
    | Some (istr, from) -> scan_inputs_buf from buf (istr :: acc)
    | None ->
      let total_len = String.length buf in
      if total_len > from then
        let rest = String.sub buf ~pos:from ~len:(total_len - from) in
        (Some rest, acc)
      else
        (None, acc)
  in
  let ibuf_size = 65536 in
  let ibuf = Bytes.create ibuf_size in
  let rec input_loop ic rem acc =
    let res = input ic ibuf 0 ibuf_size in
    if res = 0 then
      (* end of file, check if there is a remainder, and return the results *)
      match rem with
      | Some rem -> List.rev (rem :: acc)
      | None -> List.rev acc
    else
      (* new input, append remainder and scan it *)
      let actual_input = Bytes.sub_string ibuf ~pos:0 ~len:res in
      let actual_input =
        match rem with
        | None -> actual_input
        | Some rem -> rem ^ actual_input
      in
      let rem, acc = scan_inputs_buf 0 actual_input acc in
      input_loop ic rem acc
  in
  fun ic -> input_loop ic None []

let copy_channels =
  let buf_len = 65536 in
  let buf = Bytes.create buf_len in
  let rec loop ic oc =
    match input ic buf 0 buf_len with
    | 0 -> ()
    | n ->
      output oc buf 0 n;
      loop ic oc
  in
  loop

module Make (Path : sig
  type t

  val to_string : t -> string
end) =
struct
  type path = Path.t

  let open_in ?(binary = true) p =
    let fn = Path.to_string p in
    if binary then
      Stdlib.open_in_bin fn
    else
      Stdlib.open_in fn

  let open_out ?(binary = true) ?(perm = 0o666) p =
    let fn = Path.to_string p in
    let flags : Stdlib.open_flag list =
      [ Open_wronly
      ; Open_creat
      ; Open_trunc
      ; (if binary then
          Open_binary
        else
          Open_text)
      ]
    in
    Stdlib.open_out_gen flags perm fn

  let with_file_in ?binary fn ~f =
    Exn.protectx (open_in ?binary fn) ~finally:close_in ~f

  let with_file_out ?binary ?perm p ~f =
    Exn.protectx (open_out ?binary ?perm p) ~finally:close_out ~f

  let with_lexbuf_from_file fn ~f =
    with_file_in fn ~f:(fun ic ->
        let lb = Lexing.from_channel ic in
        lb.lex_curr_p <-
          { pos_fname = Path.to_string fn
          ; pos_lnum = 1
          ; pos_bol = 0
          ; pos_cnum = 0
          };
        f lb)

  let rec eagerly_input_acc ic s ~pos ~len acc =
    if len <= 0 then
      acc
    else
      let r = input ic s pos len in
      if r = 0 then
        acc
      else
        eagerly_input_acc ic s ~pos:(pos + r) ~len:(len - r) (acc + r)

  (* [eagerly_input_string ic len] tries to read [len] chars from the channel.
     Unlike [really_input_string], if the file ends before [len] characters are
     found, it returns the characters it was able to read instead of raising an
     exception.

     This can be detected by checking that the length of the resulting string is
     less than [len]. *)
  let eagerly_input_string ic len =
    let buf = Bytes.create len in
    let r = eagerly_input_acc ic buf ~pos:0 ~len 0 in
    if r = len then
      Bytes.unsafe_to_string buf
    else
      Bytes.sub_string buf ~pos:0 ~len:r

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
        (* For some files [in_channel_length] returns an invalid value. For
           instance for files in /proc it returns [0] and on Windows the
           returned value is larger than expected (it counts linebreaks as 2
           chars, even in text mode).

           To be robust in both directions, we: - use [eagerly_input_string]
           instead of [really_input_string] in case we reach the end of the file
           early - read one more character to make sure we did indeed reach the
           end of the file *)
        let s = eagerly_input_string t n in
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

  let lines_of_file fn = with_file_in fn ~f:input_lines ~binary:false

  let zero_strings_of_file fn =
    with_file_in fn ~f:input_zero_separated ~binary:true

  let write_file ?binary ?perm fn data =
    with_file_out ?binary ?perm fn ~f:(fun oc -> output_string oc data)

  let write_lines ?binary ?perm fn lines =
    with_file_out ?binary ?perm fn ~f:(fun oc ->
        List.iter
          ~f:(fun line ->
            output_string oc line;
            output_string oc "\n")
          lines)

  let read_file_and_normalize_eols fn =
    if not Stdlib.Sys.win32 then
      read_file fn
    else
      let src = read_file fn in
      let len = String.length src in
      let dst = Bytes.create len in
      let rec find_next_crnl i =
        match String.index_from src i '\r' with
        | None -> None
        | Some j ->
          if j + 1 < len && src.[j + 1] = '\n' then
            Some j
          else
            find_next_crnl (j + 1)
      in
      let rec loop src_pos dst_pos =
        match find_next_crnl src_pos with
        | None ->
          let len =
            if len > src_pos && src.[len - 1] = '\r' then
              len - 1 - src_pos
            else
              len - src_pos
          in
          Bytes.blit_string ~src ~src_pos ~dst ~dst_pos ~len;
          Bytes.sub_string dst ~pos:0 ~len:(dst_pos + len)
        | Some i ->
          let len = i - src_pos in
          Bytes.blit_string ~src ~src_pos ~dst ~dst_pos ~len;
          let dst_pos = dst_pos + len in
          Bytes.set dst dst_pos '\n';
          loop (i + 2) (dst_pos + 1)
      in
      loop 0 0

  let compare_text_files fn1 fn2 =
    let s1 = read_file_and_normalize_eols fn1 in
    let s2 = read_file_and_normalize_eols fn2 in
    String.compare s1 s2

  let compare_files fn1 fn2 =
    let s1 = read_file fn1 in
    let s2 = read_file fn2 in
    String.compare s1 s2

  let setup_copy ?(chmod = Fun.id) ~src ~dst () =
    let ic = open_in src in
    let oc =
      try
        let perm =
          (Unix.fstat (Unix.descr_of_in_channel ic)).st_perm |> chmod
        in
        Stdlib.open_out_gen
          [ Open_wronly; Open_creat; Open_trunc; Open_binary ]
          perm (Path.to_string dst)
      with
      | exn ->
        close_in ic;
        Exn.reraise exn
    in
    (ic, oc)

  let copy_file ?chmod ~src ~dst () =
    Exn.protectx (setup_copy ?chmod ~src ~dst ()) ~finally:close_both
      ~f:(fun (ic, oc) -> copy_channels ic oc)

  let file_line path n =
    with_file_in ~binary:false path ~f:(fun ic ->
        for _ = 1 to n - 1 do
          ignore (input_line ic)
        done;
        input_line ic)

  let file_lines path ~start ~stop =
    with_file_in ~binary:true path ~f:(fun ic ->
        let rec aux acc lnum =
          if lnum > stop then
            List.rev acc
          else if lnum < start then (
            ignore (input_line ic);
            aux acc (lnum + 1)
          ) else
            let line = input_line ic in
            aux ((string_of_int lnum, line) :: acc) (lnum + 1)
        in
        aux [] 1)
end

include Make (Path)

module String_path = Make (struct
  type t = string

  let to_string x = x
end)
