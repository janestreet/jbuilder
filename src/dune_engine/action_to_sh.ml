open Import

module Simplified = struct
  type destination =
    | Dev_null
    | File of string

  type source = string

  type t =
    | Run of string * string list
    | Chdir of string
    | Setenv of string * string
    | Redirect_out of t list * Action.Outputs.t * destination
    | Redirect_in of t list * Action.Inputs.t * source
    | Pipe of t list list * Action.Outputs.t
    | Sh of string
end

open Simplified

let echo s =
  let lines = String.split_lines s in
  if not (String.is_suffix s ~suffix:"\n") then
    match List.rev lines with
    | [] -> [ Run ("echo", [ "-n" ]) ]
    | last :: rest ->
      List.fold_left rest
        ~init:[ Run ("echo", [ "-n"; last ]) ]
        ~f:(fun acc s -> Run ("echo", [ s ]) :: acc)
  else
    List.map lines ~f:(fun s -> Run ("echo", [ s ]))

let cat fn = Run ("cat", [ fn ])

let mkdir p = Run ("mkdir", [ "-p"; p ])

let interpret_perm (perm : Action.File_perm.t) fn acc =
  match perm with
  | Normal -> acc
  | Executable -> Run ("chmod", [ "+x"; fn ]) :: acc

let simplify act =
  let rec loop (act : Action.For_shell.t) acc =
    match act with
    | Run (prog, args) -> Run (prog, args) :: acc
    | With_accepted_exit_codes (_, t) -> loop t acc
    | Dynamic_run (prog, args) -> Run (prog, args) :: acc
    | Chdir (p, act) -> loop act (Chdir p :: mkdir p :: acc)
    | Setenv (k, v, act) -> loop act (Setenv (k, v) :: acc)
    | Redirect_out (outputs, fn, perm, act) ->
      interpret_perm perm fn (Redirect_out (block act, outputs, File fn) :: acc)
    | Redirect_in (inputs, fn, act) ->
      Redirect_in (block act, inputs, fn) :: acc
    | Ignore (outputs, act) ->
      Redirect_out (block act, outputs, Dev_null) :: acc
    | Progn l -> List.fold_left l ~init:acc ~f:(fun acc act -> loop act acc)
    | Echo xs -> echo (String.concat xs ~sep:"")
    | Cram script -> echo (sprintf "cram %s" script)
    | Cat x -> cat x :: acc
    | Copy (x, y) -> Run ("cp", [ x; y ]) :: acc
    | Symlink (x, y) ->
      Run ("ln", [ "-s"; x; y ]) :: Run ("rm", [ "-f"; y ]) :: acc
    | Hardlink (x, y) -> Run ("ln", [ x; y ]) :: Run ("rm", [ "-f"; y ]) :: acc
    | Copy_and_add_line_directive (x, y) ->
      Redirect_out
        ( echo (Utils.line_directive ~filename:x ~line_number:1) @ [ cat x ]
        , Stdout
        , File y )
      :: acc
    | System x -> Sh x :: acc
    | Bash x -> Run ("bash", [ "-e"; "-u"; "-o"; "pipefail"; "-c"; x ]) :: acc
    | Write_file (x, perm, y) ->
      interpret_perm perm x (Redirect_out (echo y, Stdout, File x) :: acc)
    | Rename (x, y) -> Run ("mv", [ x; y ]) :: acc
    | Remove_tree x -> Run ("rm", [ "-rf"; x ]) :: acc
    | Mkdir x -> mkdir x :: acc
    | Diff { optional; file1; file2; mode = Binary } ->
      assert (not optional);
      Run ("cmp", [ file1; file2 ]) :: acc
    | Diff { optional = true; file1; file2; mode = _ } ->
      Sh
        (Printf.sprintf "test ! -e file1 -o ! -e file2 || diff %s %s"
           (String.quote_for_shell file1)
           (String.quote_for_shell file2))
      :: acc
    | Diff { optional = false; file1; file2; mode = _ } ->
      Run ("diff", [ file1; file2 ]) :: acc
    | Merge_files_into (srcs, extras, target) ->
      Sh
        (Printf.sprintf "{ echo -ne %s; cat %s; } | sort -u > %s"
           (Filename.quote
              (List.map extras ~f:(sprintf "%s\n") |> String.concat ~sep:""))
           (List.map srcs ~f:String.quote_for_shell |> String.concat ~sep:" ")
           (String.quote_for_shell target))
      :: acc
    | No_infer act -> loop act acc
    | Pipe (outputs, l) -> Pipe (List.map ~f:block l, outputs) :: acc
    | Format_dune_file (ver, src, dst) ->
      Redirect_out
        ( [ Run
              ( "dune"
              , [ "format-dune-file"
                ; "--dune-version"
                ; Dune_lang.Syntax.Version.to_string ver
                ; src
                ] )
          ]
        , Stdout
        , File dst )
      :: acc
  and block act =
    match List.rev (loop act []) with
    | [] -> [ Run ("true", []) ]
    | l -> l
  in
  block act

let quote s = Pp.verbatim (String.quote_for_shell s)

let rec block l =
  match l with
  | [ x ] -> pp x
  | l ->
    Pp.box
      (Pp.concat
         [ Pp.hvbox ~indent:2
             (Pp.concat
                [ Pp.char '{'
                ; Pp.space
                ; Pp.hvbox
                    (Pp.concat_map l ~sep:Pp.space ~f:(fun x ->
                         Pp.seq (pp x) (Pp.char ';')))
                ])
         ; Pp.space
         ; Pp.char '}'
         ])

and pp = function
  | Run (prog, args) ->
    Pp.hovbox ~indent:2
      (Pp.concat
         (quote prog
         :: List.concat_map args ~f:(fun arg -> [ Pp.space; quote arg ])))
  | Chdir dir ->
    Pp.hovbox ~indent:2 (Pp.concat [ Pp.verbatim "cd"; Pp.space; quote dir ])
  | Setenv (k, v) -> Pp.concat [ Pp.verbatim k; Pp.verbatim "="; quote v ]
  | Sh s -> Pp.verbatim s
  | Redirect_in (l, inputs, src) ->
    let body = block l in
    Pp.hovbox ~indent:2
      (Pp.concat
         [ body
         ; Pp.space
         ; Pp.verbatim
             (match inputs with
             | Stdin -> "<")
         ; Pp.space
         ; quote src
         ])
  | Redirect_out (l, outputs, dest) ->
    let body = block l in
    Pp.hovbox ~indent:2
      (Pp.concat
         [ body
         ; Pp.space
         ; Pp.verbatim
             (match outputs with
             | Stdout -> ">"
             | Stderr -> "2>"
             | Outputs -> "&>")
         ; Pp.space
         ; quote
             (match dest with
             | Dev_null -> "/dev/null"
             | File fn -> fn)
         ])
  | Pipe (l, outputs) -> (
    let first_pipe, end_ =
      match outputs with
      | Stdout -> (" | ", "")
      | Outputs -> (" 2>&1 | ", "")
      | Stderr -> (" 2> >( ", " 1>&2 )")
    in
    match l with
    | [] -> assert false
    | first :: l ->
      Pp.hovbox ~indent:2
        (Pp.concat ~sep:Pp.space
           [ block first
           ; Pp.verbatim first_pipe
           ; Pp.concat ~sep:(Pp.verbatim " | ") (List.map l ~f:block)
           ; Pp.verbatim end_
           ]))

let rec pp_seq = function
  | [] -> Pp.verbatim "true"
  | [ x ] -> pp x
  | x :: l -> Pp.concat [ pp x; Pp.char ';'; Pp.cut; pp_seq l ]

let pp act = pp_seq (simplify act)
