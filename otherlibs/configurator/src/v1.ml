open Import

let die = die

type t =
  { name : string
  ; dest_dir : string
  ; ocamlc : string
  ; log : string -> unit
  ; mutable counter : int
  ; ext_obj : string
  ; c_compiler : string
  ; stdlib_dir : string
  ; ccomp_type : string
  ; c_libraries : string list
  ; ocamlc_config : Ocaml_config.Vars.t
  ; ocamlc_config_cmd : string
  }

let rec rm_rf dir =
  Array.iter (Sys.readdir dir) ~f:(fun fn ->
      let fn = dir ^/ fn in
      if Sys.is_directory fn then
        rm_rf fn
      else
        Unix.unlink fn);
  Unix.rmdir dir

module Temp = struct
  (* Copied from filename.ml and adapted for directories *)

  let prng = lazy (Random.State.make_self_init ())

  let gen_name ~temp_dir ~prefix ~suffix =
    let rnd = Random.State.bits (Lazy.force prng) land 0xFFFFFF in
    temp_dir ^/ Printf.sprintf "%s%06x%s" prefix rnd suffix

  let create ~prefix ~suffix ~mk =
    let temp_dir = Filename.get_temp_dir_name () in
    let rec try_name counter =
      let name = gen_name ~temp_dir ~prefix ~suffix in
      match mk name with
      | () -> name
      | exception Unix.Unix_error _ when counter < 1000 -> try_name (counter + 1)
    in
    try_name 0

  let create_temp_dir ~prefix ~suffix =
    let dir = create ~prefix ~suffix ~mk:(fun name -> Unix.mkdir name 0o700) in
    at_exit (fun () -> rm_rf dir);
    dir
end

module Flags = struct
  let extract_words = String.extract_words

  let extract_comma_space_separated_words =
    String.extract_comma_space_separated_words

  let extract_blank_separated_words = String.extract_blank_separated_words

  let write_lines path s = Io.write_lines path s

  let write_sexp path s =
    let sexp =
      Dune_lang.List (List.map s ~f:(fun s -> Dune_lang.Quoted_string s))
    in
    Io.write_file path (Dune_lang.to_string sexp)
end

module Find_in_path = struct
  let path_sep =
    if Sys.win32 then
      ';'
    else
      ':'

  let get_path () =
    match Sys.getenv "PATH" with
    | exception Not_found -> []
    | s -> String.split s ~on:path_sep

  let exe =
    if Sys.win32 then
      ".exe"
    else
      ""

  let prog_not_found prog = die "Program %s not found in PATH" prog

  let best_prog dir prog =
    let fn = dir ^/ prog ^ ".opt" ^ exe in
    if Sys.file_exists fn then
      Some fn
    else
      let fn = dir ^/ prog ^ exe in
      if Sys.file_exists fn then
        Some fn
      else
        None

  let find_ocaml_prog prog =
    match List.find_map (get_path ()) ~f:(fun dir -> best_prog dir prog) with
    | None -> prog_not_found prog
    | Some fn -> fn

  let which prog =
    List.find_map (get_path ()) ~f:(fun dir ->
        let fn = dir ^/ prog ^ exe in
        Option.some_if (Sys.file_exists fn) fn)
end

let logf t fmt = Printf.ksprintf t.log fmt

let gen_id t =
  let n = t.counter in
  t.counter <- n + 1;
  n

let quote_if_needed =
  let need_quote = function
    | ' '
    | '\"' ->
      true
    | _ -> false
  in
  fun s ->
    if String.is_empty s || String.exists ~f:need_quote s then
      Filename.quote s
    else
      s

module Process = struct
  type result =
    { exit_code : int
    ; stdout : string
    ; stderr : string
    }

  let command_line prog args =
    String.concat ~sep:" " (List.map (prog :: args) ~f:quote_if_needed)

  let run_process t ?dir ?env prog args =
    let prog_command_line = command_line prog args in
    logf t "run: %s" prog_command_line;
    let n = gen_id t in
    let create_process =
      let args = Array.of_list (prog :: args) in
      match env with
      | None -> Unix.create_process prog args
      | Some env ->
        let env = Array.of_list env in
        Unix.create_process_env prog args env
    in
    let stdout_fn = t.dest_dir ^/ sprintf "stdout-%d" n in
    let stderr_fn = t.dest_dir ^/ sprintf "stderr-%d" n in
    let status =
      let run () =
        let openfile f =
          Unix.openfile f [ O_WRONLY; O_CREAT; O_TRUNC; O_SHARE_DELETE ] 0o666
        in
        let stdout = openfile stdout_fn in
        let stderr = openfile stderr_fn in
        let stdin, stdin_w = Unix.pipe () in
        Unix.close stdin_w;
        let p = create_process stdin stdout stderr in
        Unix.close stdin;
        Unix.close stdout;
        Unix.close stderr;
        let _pid, status = Unix.waitpid [] p in
        status
      in
      match dir with
      | None -> run ()
      | Some d ->
        let old_dir = Sys.getcwd () in
        Exn.protect
          ~f:(fun () ->
            Sys.chdir d;
            run ())
          ~finally:(fun () -> Sys.chdir old_dir)
    in
    match status with
    | Unix.WSIGNALED signal ->
      die "signal %d killed process: %s" signal prog_command_line
    | WSTOPPED signal ->
      die "signal %d stopped process: %s" signal prog_command_line
    | WEXITED exit_code ->
      logf t "-> process exited with code %d" exit_code;
      let stdout = Io.read_file stdout_fn in
      let stderr = Io.read_file stderr_fn in
      logf t "-> stdout:";
      List.iter (String.split_lines stdout) ~f:(logf t " | %s");
      logf t "-> stderr:";
      List.iter (String.split_lines stderr) ~f:(logf t " | %s");
      { exit_code; stdout; stderr }

  (* [cmd] which cannot be quoted (such as [t.c_compiler] which contains some
     flags) followed by additional arguments. *)
  let command_args cmd args =
    String.concat ~sep:" " (cmd :: List.map args ~f:quote_if_needed)

  let run_command t ?dir ?(env = []) cmd =
    logf t "run: %s" cmd;
    let n = gen_id t in
    let stdout_fn = t.dest_dir ^/ sprintf "stdout-%d" n in
    let stderr_fn = t.dest_dir ^/ sprintf "stderr-%d" n in
    let in_dir =
      match dir with
      | None -> ""
      | Some dir -> sprintf "cd %s && " (Filename.quote dir)
    in
    let with_env =
      match env with
      | [] -> ""
      | _ -> "env " ^ String.concat ~sep:" " env
    in
    let exit_code =
      Printf.ksprintf Sys.command "%s%s %s > %s 2> %s" in_dir with_env cmd
        (Filename.quote stdout_fn) (Filename.quote stderr_fn)
    in
    let stdout = Io.read_file stdout_fn in
    let stderr = Io.read_file stderr_fn in
    logf t "-> process exited with code %d" exit_code;
    logf t "-> stdout:";
    List.iter (String.split_lines stdout) ~f:(logf t " | %s");
    logf t "-> stderr:";
    List.iter (String.split_lines stderr) ~f:(logf t " | %s");
    { exit_code; stdout; stderr }

  let run_command_capture_exn t ?dir ?env cmd =
    let { exit_code; stdout; stderr } = run_command t ?dir ?env cmd in
    if exit_code <> 0 then
      die "command exited with code %d: %s" exit_code cmd
    else if not (String.is_empty stderr) then
      die "command has non-empty stderr: %s" cmd
    else
      stdout

  let run_command_ok t ?dir ?env cmd =
    (run_command t ?dir ?env cmd).exit_code = 0

  let run t ?dir ?env prog args =
    run_command t ?dir ?env (command_line prog args)

  let run_capture_exn t ?dir ?env prog args =
    run_command_capture_exn t ?dir ?env (command_line prog args)

  let run_ok t ?dir ?env prog args =
    run_command_ok t ?dir ?env (command_line prog args)
end

let ocaml_config_var t var = Ocaml_config.Vars.find t.ocamlc_config var

let ocaml_config_var_exn t var =
  match Ocaml_config.Vars.find t.ocamlc_config var with
  | None ->
    die "variable %S not found in the output of `%s`" var t.ocamlc_config_cmd
  | Some s -> s

type config =
  { ocamlc : string
  ; vars : Ocaml_config.Vars.t
  }

let dune_is_too_old ~min:v =
  die
    "You seem to be running dune < %s. This version of dune-configurator \
     requires at least dune %s."
    v v

let read_dot_dune_configurator_file ~build_dir =
  let file = Filename.concat build_dir ".dune/configurator.v2" in
  if not (Sys.file_exists file) then dune_is_too_old ~min:"2.6";
  let open Sexp in
  let unable_to_parse err = die "Unable to parse %S.@.%s@." file err in
  let sexp =
    match Io.with_file_in file ~f:Sexp.input with
    | Ok s -> s
    | Error e -> unable_to_parse e
  in
  match sexp with
  | Atom _ -> unable_to_parse "unexpected atom"
  | List xs ->
    let field name =
      match
        List.find_map xs ~f:(function
          | List [ Atom name'; f ] when name = name' -> Some f
          | _ -> None)
      with
      | None -> die "unable to find field %S" name
      | Some f -> f
    in
    let ocamlc =
      match field "ocamlc" with
      | Atom o -> o
      | _ -> die "invalid ocamlc field"
    in
    let vars =
      let bindings =
        match field "ocaml_config_vars" with
        | List bindings ->
          List.map bindings ~f:(function
            | List [ Atom k; Atom v ] -> (k, v)
            | _ -> die "invalid output")
        | _ -> die "invalid output"
      in
      Ocaml_config.Vars.of_list_exn bindings
    in
    { ocamlc; vars }

let fill_in_fields_that_depends_on_ocamlc_config t =
  let get = ocaml_config_var_exn t in
  let get_flags var =
    get var |> String.trim |> Flags.extract_blank_separated_words
  in
  let c_compiler, c_libraries =
    match Ocaml_config.Vars.find t.ocamlc_config "c_compiler" with
    | Some c_comp ->
      (c_comp ^ " " ^ get "ocamlc_cflags", get_flags "native_c_libraries")
    | None -> (get "bytecomp_c_compiler", get_flags "bytecomp_c_libraries")
  in
  { t with
    ext_obj = get "ext_obj"
  ; c_compiler
  ; stdlib_dir = get "standard_library"
  ; ccomp_type = get "ccomp_type"
  ; c_libraries
  }

let create_from_inside_dune ~dest_dir ~log ~build_dir ~name =
  let dest_dir =
    match dest_dir with
    | Some dir -> dir
    | None -> Temp.create_temp_dir ~prefix:"ocaml-configurator" ~suffix:""
  in
  let { ocamlc; vars = ocamlc_config } =
    read_dot_dune_configurator_file ~build_dir
  in
  let ocamlc_config_cmd = Process.command_line ocamlc [ "-config" ] in
  fill_in_fields_that_depends_on_ocamlc_config
    { name
    ; ocamlc
    ; log
    ; dest_dir
    ; counter = 0
    ; ocamlc_config
    ; ocamlc_config_cmd
    ; ext_obj = ""
    ; c_compiler = ""
    ; stdlib_dir = ""
    ; ccomp_type = ""
    ; c_libraries = []
    }

let create ?dest_dir ?ocamlc ?(log = ignore) name =
  match (ocamlc, Option.try_with (fun () -> Sys.getenv "INSIDE_DUNE")) with
  | None, Some build_dir when build_dir <> "1" ->
    create_from_inside_dune ~dest_dir ~log ~build_dir ~name
  | _ ->
    let dest_dir =
      match dest_dir with
      | Some dir -> dir
      | None -> Temp.create_temp_dir ~prefix:"ocaml-configurator" ~suffix:""
    in
    let ocamlc =
      match ocamlc with
      | Some fn -> fn
      | None -> Find_in_path.find_ocaml_prog "ocamlc"
    in
    let ocamlc_config_cmd = Process.command_line ocamlc [ "-config" ] in
    let t =
      { name
      ; ocamlc
      ; log
      ; dest_dir
      ; counter = 0
      ; ext_obj = ""
      ; c_compiler = ""
      ; stdlib_dir = ""
      ; ccomp_type = ""
      ; c_libraries = []
      ; ocamlc_config = Ocaml_config.Vars.of_list_exn []
      ; ocamlc_config_cmd
      }
    in
    let ocamlc_config =
      let ocamlc_config_output =
        Process.run_command_capture_exn t ~dir:dest_dir ocamlc_config_cmd
        |> String.split_lines
      in
      match Ocaml_config.Vars.of_lines ocamlc_config_output with
      | Ok x -> x
      | Error msg ->
        die "Failed to parse the output of '%s':@\n%s" ocamlc_config_cmd msg
    in
    fill_in_fields_that_depends_on_ocamlc_config { t with ocamlc_config }

let need_to_compile_and_link_separately t =
  (* Vague memory from writing the discover.ml script for Lwt... *)
  match t.ccomp_type with
  | "msvc" -> true
  | _ -> false

let compile_and_link_c_prog t ?(c_flags = []) ?(link_flags = []) code =
  let dir = t.dest_dir ^/ sprintf "c-test-%d" (gen_id t) in
  Unix.mkdir dir 0o777;
  let base = dir ^/ "test" in
  let c_fname = base ^ ".c" in
  let obj_fname = base ^ t.ext_obj in
  let exe_fname = base ^ ".exe" in
  Io.write_file c_fname code;
  logf t "compiling c program:";
  List.iter (String.split_lines code) ~f:(logf t " | %s");
  let run_ok args =
    Process.run_command_ok t ~dir (Process.command_args t.c_compiler args)
  in
  let ok =
    if need_to_compile_and_link_separately t then
      run_ok (c_flags @ [ "-I"; t.stdlib_dir; "-c"; c_fname ])
      && run_ok (("-o" :: exe_fname :: obj_fname :: t.c_libraries) @ link_flags)
    else
      run_ok
        (List.concat
           [ c_flags
           ; [ "-I"; t.stdlib_dir; "-o"; exe_fname; c_fname ]
           ; t.c_libraries
           ; link_flags
           ])
  in
  if ok then
    Ok ()
  else
    Error ()

let compile_c_prog t ?(c_flags = []) code =
  let dir = t.dest_dir ^/ sprintf "c-test-%d" (gen_id t) in
  Unix.mkdir dir 0o777;
  let base = dir ^/ "test" in
  let c_fname = base ^ ".c" in
  let obj_fname = base ^ t.ext_obj in
  Io.write_file c_fname code;
  logf t "compiling c program:";
  List.iter (String.split_lines code) ~f:(logf t " | %s");
  let ok =
    Process.run_command_ok t ~dir
      (Process.command_args t.c_compiler
         (c_flags
         @ "-I" :: t.stdlib_dir :: "-o" :: obj_fname :: "-c" :: c_fname
           :: t.c_libraries))
  in
  if ok then
    Ok obj_fname
  else
    Error ()

let c_test t ?c_flags ?link_flags code =
  match compile_and_link_c_prog t ?c_flags ?link_flags code with
  | Ok _ -> true
  | Error _ -> false

module C_define = struct
  module Type = struct
    type t =
      | Switch
      | Int
      | String

    let name = function
      | Switch -> "bool"
      | Int -> "int"
      | String -> "string"
  end

  module Value = struct
    type t =
      | Switch of bool
      | Int of int
      | String of string
  end

  let extract_program ?prelude includes vars =
    let has_type t = List.exists vars ~f:(fun (_, t') -> t = t') in
    let buf = Buffer.create 1024 in
    let pr fmt = Printf.bprintf buf (fmt ^^ "\n") in
    List.iter includes ~f:(pr "#include <%s>");
    pr "";
    Option.iter prelude ~f:(pr "%s");
    if has_type Type.Int then
      pr
        {|
#define DUNE_ABS(x) ((x >= 0)? x: -(x))
#define DUNE_D0(x) ('0'+(DUNE_ABS(x)/1         )%%10)
#define DUNE_D1(x) ('0'+(DUNE_ABS(x)/10        )%%10), DUNE_D0(x)
#define DUNE_D2(x) ('0'+(DUNE_ABS(x)/100       )%%10), DUNE_D1(x)
#define DUNE_D3(x) ('0'+(DUNE_ABS(x)/1000      )%%10), DUNE_D2(x)
#define DUNE_D4(x) ('0'+(DUNE_ABS(x)/10000     )%%10), DUNE_D3(x)
#define DUNE_D5(x) ('0'+(DUNE_ABS(x)/100000    )%%10), DUNE_D4(x)
#define DUNE_D6(x) ('0'+(DUNE_ABS(x)/1000000   )%%10), DUNE_D5(x)
#define DUNE_D7(x) ('0'+(DUNE_ABS(x)/10000000  )%%10), DUNE_D6(x)
#define DUNE_D8(x) ('0'+(DUNE_ABS(x)/100000000 )%%10), DUNE_D7(x)
#define DUNE_D9(x) ('0'+(DUNE_ABS(x)/1000000000)%%10), DUNE_D8(x)
#define DUNE_SIGN(x) ((x >= 0)? '0': '-')
|};
    List.iteri vars ~f:(fun i (name, t) ->
        match t with
        | Type.Int ->
          let c_arr_i =
            let b = Buffer.create 8 in
            let is = string_of_int i in
            for i = 0 to String.length is - 1 do
              Printf.bprintf b "'%c', " is.[i]
            done;
            Buffer.contents b
          in
          pr
            {|
const char s%i[] = {
  'B', 'E', 'G', 'I', 'N', '-', %s'-',
  DUNE_SIGN((%s)),
  DUNE_D9((%s)),
  '-', 'E', 'N', 'D'
};
|}
            i c_arr_i name name
        | String -> pr {|const char *s%i = "BEGIN-%i-" %s "-END";|} i i name
        | Switch ->
          pr
            {|
#ifdef %s
const char *s%i = "BEGIN-%i-true-END";
#else
const char *s%i = "BEGIN-%i-false-END";
#endif
|}
            name i i i i);
    Buffer.contents buf

  let extract_values obj_file vars =
    let values =
      Io.with_lexbuf_from_file obj_file ~f:(Extract_obj.extract [])
      |> List.fold_left ~init:Int.Map.empty ~f:(fun acc (key, v) ->
             Int.Map.update acc key ~f:(function
               | None -> Some [ v ]
               | Some vs -> Some (v :: vs)))
    in
    List.mapi vars ~f:(fun i (name, t) ->
        let raw_vals =
          match Int.Map.find values i with
          | Some v -> v
          | None -> die "Unable to get value for %s" name
        in
        let parse_val_or_exn f =
          let f x =
            match f x with
            | Some s -> s
            | None ->
              die
                "Unable to read variable %S of type %s. Invalid value %S in %s \
                 found"
                name (Type.name t) x obj_file
          in
          let vs =
            List.map ~f:(fun x -> (x, f x)) raw_vals
            |> List.sort_uniq ~cmp:(fun (_, x) (_, y) -> compare x y)
          in
          match vs with
          | [] -> assert false
          | [ (_, v) ] -> v
          | vs ->
            let vs = List.map ~f:fst vs in
            die "Duplicate values for %s:\n%s" name
              (vs |> List.map ~f:(sprintf "- %s") |> String.concat ~sep:"\n")
        in
        let value =
          match t with
          | Type.Switch -> Value.Switch (parse_val_or_exn Bool.of_string)
          | Int -> Value.Int (parse_val_or_exn Int.of_string)
          | String -> String (parse_val_or_exn Option.some)
        in
        (name, value))

  let import t ?prelude ?c_flags ~includes vars =
    let program = extract_program ?prelude ("stdio.h" :: includes) vars in
    match compile_c_prog t ?c_flags program with
    | Error _ -> die "failed to compile program"
    | Ok obj -> extract_values obj vars

  let gen_header_file t ~fname ?protection_var vars =
    let protection_var =
      match protection_var with
      | Some v -> v
      | None ->
        String.map
          (t.name ^ "_" ^ Filename.basename fname)
          ~f:(function
            | 'a' .. 'z' as c -> Char.uppercase_ascii c
            | ('A' .. 'Z' | '0' .. '9') as c -> c
            | _ -> '_')
    in
    let vars = List.sort vars ~cmp:(fun (a, _) (b, _) -> compare a b) in
    let lines =
      List.map vars ~f:(fun (name, value) ->
          match (value : Value.t) with
          | Switch false -> sprintf "#undef  %s" name
          | Switch true -> sprintf "#define %s" name
          | Int n -> sprintf "#define %s (%d)" name n
          | String s -> sprintf "#define %s %S" name s)
    in
    let lines =
      List.concat
        [ [ sprintf "#ifndef %s" protection_var
          ; sprintf "#define %s" protection_var
          ]
        ; lines
        ; [ "#endif" ]
        ]
    in
    logf t "writing header file %s" fname;
    List.iter lines ~f:(logf t " | %s");
    let tmp_fname = fname ^ ".tmp" in
    Io.write_lines tmp_fname lines;
    Sys.rename tmp_fname fname
end

let which t prog =
  logf t "which: %s" prog;
  let x = Find_in_path.which prog in
  logf t "-> %s"
    (match x with
    | None -> "not found"
    | Some fn -> "found: " ^ quote_if_needed fn);
  x

module Pkg_config = struct
  type nonrec t =
    { pkg_config : string
    ; configurator : t
    }

  let get c =
    Option.map (which c "pkg-config") ~f:(fun pkg_config ->
        { pkg_config; configurator = c })

  type package_conf =
    { libs : string list
    ; cflags : string list
    }

  let gen_query t ~package ~expr =
    let c = t.configurator in
    let dir = c.dest_dir in
    let expr =
      match expr with
      | Some e -> e
      | None ->
        if
          String.exists package ~f:(function
            | '='
            | '>'
            | '<' ->
              true
            | _ -> false)
        then
          warn
            "Package name %S contains invalid characters. Use \
             Pkg_config.query_expr to construct proper queries"
            package;
        package
    in
    let env =
      match ocaml_config_var c "system" with
      | Some "macosx" ->
        let open Option.O in
        which c "brew" >>= fun brew ->
        let new_pkg_config_path =
          let prefix =
            String.trim (Process.run_capture_exn c ~dir brew [ "--prefix" ])
          in
          let p =
            sprintf "%s/opt/%s/lib/pkgconfig" (quote_if_needed prefix) package
          in
          Option.some_if
            (match Sys.is_directory p with
            | s -> s
            | exception Sys_error _ -> false)
            p
        in
        new_pkg_config_path >>| fun new_pkg_config_path ->
        let _PKG_CONFIG_PATH = "PKG_CONFIG_PATH" in
        let pkg_config_path =
          match Sys.getenv _PKG_CONFIG_PATH with
          | s -> s ^ ":"
          | exception Not_found -> ""
        in
        [ sprintf "%s=%s%s" _PKG_CONFIG_PATH pkg_config_path new_pkg_config_path
        ]
      | _ -> None
    in
    let pc_flags = "--print-errors" in
    let { Process.exit_code; stderr; _ } =
      Process.run_process c ~dir ?env t.pkg_config [ pc_flags; expr ]
    in
    if exit_code = 0 then
      let run what =
        match
          String.trim
            (Process.run_capture_exn c ~dir ?env t.pkg_config [ what; package ])
        with
        | "" -> []
        | s -> String.extract_blank_separated_words s
      in
      Ok { libs = run "--libs"; cflags = run "--cflags" }
    else
      Error stderr

  let query t ~package = Result.to_option @@ gen_query t ~package ~expr:None

  let query_expr t ~package ~expr =
    Result.to_option @@ gen_query t ~package ~expr:(Some expr)

  let query_expr_err t ~package ~expr = gen_query t ~package ~expr:(Some expr)
end

let main ?(args = []) ~name f =
  let build_dir =
    match Sys.getenv "INSIDE_DUNE" with
    | exception Not_found ->
      die
        "Configurator scripts must be run with Dune. To manually run a script, \
         use $ dune exec."
    | "1" -> dune_is_too_old ~min:"2.3"
    | s -> s
  in
  let verbose = ref false in
  let dest_dir = ref None in
  let args =
    Arg.align
      ([ ("-verbose", Arg.Set verbose, " be verbose")
       ; ( "-dest-dir"
         , Arg.String (fun s -> dest_dir := Some s)
         , "DIR save temporary files to this directory" )
       ]
      @ args)
  in
  let anon s = raise (Arg.Bad (sprintf "don't know what to do with %s" s)) in
  let usage = sprintf "%s [OPTIONS]" (Filename.basename Sys.executable_name) in
  Arg.parse args anon usage;
  let log_db = ref [] in
  let log s = log_db := s :: !log_db in
  try
    let t =
      create_from_inside_dune ~dest_dir:!dest_dir
        ~log:
          (if !verbose then
            prerr_endline
          else
            log)
        ~build_dir ~name
    in
    f t
  with
  | exn -> (
    let bt = Printexc.get_raw_backtrace () in
    List.iter (List.rev !log_db) ~f:(eprintf "%s\n");
    match exn with
    | Fatal_error msg ->
      eprintf "Error: %s\n%!" msg;
      exit 1
    | _ -> Exn.raise_with_backtrace exn bt)
