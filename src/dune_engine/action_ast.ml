open! Stdune
open Import
open Dune_lang.Decoder

let () =
  let module M = Stanza in
  ()

module Outputs = struct
  include Action_intf.Outputs

  let to_string = function
    | Stdout -> "stdout"
    | Stderr -> "stderr"
    | Outputs -> "outputs"
end

module Inputs = struct
  include Action_intf.Inputs

  let to_string = function
    | Stdin -> "stdin"
end

module type Target_intf = sig
  include Dune_lang.Conv.S

  val is_dev_null : t -> bool
end

module File_perm = struct
  include Action_intf.File_perm

  let suffix = function
    | Normal -> ""
    | Executable -> "-executable"
end

module Make
    (Program : Dune_lang.Conv.S)
    (Path : Dune_lang.Conv.S)
    (Target : Target_intf)
    (String : Dune_lang.Conv.S)
    (Ast : Action_intf.Ast
             with type program := Program.t
             with type path := Path.t
             with type target := Target.t
             with type string := String.t) =
struct
  include Ast

  let translate_to_ignore fn output action =
    if Target.is_dev_null fn then
      Ignore (output, action)
    else
      Redirect_out (output, fn, Normal, action)

  let two_or_more decode =
    let open Dune_lang.Decoder in
    let+ n1 = decode
    and+ n2 = decode
    and+ rest = repeat decode in
    n1 :: n2 :: rest

  let decode =
    let path = Path.decode in
    let string = String.decode in
    let target = Target.decode in
    Dune_lang.Decoder.fix (fun t ->
        sum
          [ ( "run"
            , let+ prog = Program.decode
              and+ args = repeat String.decode in
              Run (prog, args) )
          ; ( "with-accepted-exit-codes"
            , let open Dune_lang in
              Syntax.since Stanza.syntax (2, 0)
              >>> let+ codes = Predicate_lang.decode_one Dune_lang.Decoder.int
                  and+ version = Syntax.get_exn Stanza.syntax
                  and+ loc, t = located t in
                  let nesting_support_version = (2, 2) in
                  let nesting_support =
                    Syntax.Version.Infix.(version >= nesting_support_version)
                  in
                  let rec is_ok = function
                    | Run _
                    | Bash _
                    | System _ ->
                      true
                    | Chdir (_, t)
                    | Setenv (_, _, t)
                    | Ignore (_, t)
                    | Redirect_in (_, _, t)
                    | Redirect_out (_, _, _, t)
                    | No_infer t ->
                      if nesting_support then
                        is_ok t
                      else
                        Syntax.Error.since loc Stanza.syntax
                          nesting_support_version
                          ~what:
                            "nesting modifiers under 'with-accepted-exit-codes'"
                    | _ -> false
                  in
                  let quote = List.map ~f:(Printf.sprintf "\"%s\"") in
                  match (is_ok t, nesting_support) with
                  | true, _ -> With_accepted_exit_codes (codes, t)
                  | false, true ->
                    User_error.raise ~loc
                      [ Pp.textf
                          "Only %s can be nested under \
                           \"with-accepted-exit-codes\""
                          (Stdune.String.enumerate_and
                             (quote
                                [ "run"
                                ; "bash"
                                ; "system"
                                ; "chdir"
                                ; "setenv"
                                ; "ignore-<outputs>"
                                ; "with-stdin-from"
                                ; "with-<outputs>-to"
                                ; "no-infer"
                                ]))
                      ]
                  | false, false ->
                    User_error.raise ~loc
                      [ Pp.textf
                          "with-accepted-exit-codes can only be used with %s"
                          (Stdune.String.enumerate_or
                             (quote [ "run"; "bash"; "system" ]))
                      ] )
          ; ( "dynamic-run"
            , Dune_lang.Syntax.since Action_plugin.syntax (0, 1)
              >>> let+ prog = Program.decode
                  and+ args = repeat String.decode in
                  Dynamic_run (prog, args) )
          ; ( "chdir"
            , let+ dn = path
              and+ t = t in
              Chdir (dn, t) )
          ; ( "setenv"
            , let+ k = string
              and+ v = string
              and+ t = t in
              Setenv (k, v, t) )
          ; ( "with-stdout-to"
            , let+ fn = target
              and+ t = t in
              translate_to_ignore fn Stdout t )
          ; ( "with-stderr-to"
            , let+ fn = target
              and+ t = t in
              translate_to_ignore fn Stderr t )
          ; ( "with-outputs-to"
            , let+ fn = target
              and+ t = t in
              translate_to_ignore fn Outputs t )
          ; ( "with-stdin-from"
            , Dune_lang.Syntax.since Stanza.syntax (2, 0)
              >>> let+ fn = path
                  and+ t = t in
                  Redirect_in (Stdin, fn, t) )
          ; ("ignore-stdout", t >>| fun t -> Ignore (Stdout, t))
          ; ("ignore-stderr", t >>| fun t -> Ignore (Stderr, t))
          ; ("ignore-outputs", t >>| fun t -> Ignore (Outputs, t))
          ; ("progn", repeat t >>| fun l -> Progn l)
          ; ( "echo"
            , let+ x = string
              and+ xs = repeat string in
              Echo (x :: xs) )
          ; ("cat", path >>| fun x -> Cat x)
          ; ( "copy"
            , let+ src = path
              and+ dst = target in
              Copy (src, dst) )
          ; ( "copy#"
            , let+ src = path
              and+ dst = target in
              Copy_and_add_line_directive (src, dst) )
          ; ( "copy-and-add-line-directive"
            , let+ src = path
              and+ dst = target in
              Copy_and_add_line_directive (src, dst) )
          ; ("system", string >>| fun cmd -> System cmd)
          ; ("bash", string >>| fun cmd -> Bash cmd)
          ; ( "write-file"
            , let+ fn = target
              and+ s = string in
              Write_file (fn, Normal, s) )
          ; ( "diff"
            , let+ diff = Diff.decode path target ~optional:false in
              Diff diff )
          ; ( "diff?"
            , let+ diff = Diff.decode path target ~optional:true in
              Diff diff )
          ; ( "cmp"
            , let+ diff = Diff.decode_binary path target in
              Diff diff )
          ; ( "no-infer"
            , Dune_lang.Syntax.since Stanza.syntax (2, 6) >>> t >>| fun t ->
              No_infer t )
          ; ( "pipe-stdout"
            , Dune_lang.Syntax.since Stanza.syntax (2, 7)
              >>> let+ ts = two_or_more t in
                  Pipe (Stdout, ts) )
          ; ( "pipe-stderr"
            , Dune_lang.Syntax.since Stanza.syntax (2, 7)
              >>> let+ ts = two_or_more t in
                  Pipe (Stderr, ts) )
          ; ( "pipe-outputs"
            , Dune_lang.Syntax.since Stanza.syntax (2, 7)
              >>> let+ ts = two_or_more t in
                  Pipe (Outputs, ts) )
          ; ( "cram"
            , Dune_lang.Syntax.since Stanza.syntax (2, 7)
              >>> let+ script = path in
                  Cram script )
          ])

  let rec encode =
    let open Dune_lang in
    let program = Program.encode in
    let string = String.encode in
    let path = Path.encode in
    let target = Target.encode in
    function
    | Run (a, xs) -> List (atom "run" :: program a :: List.map xs ~f:string)
    | With_accepted_exit_codes (pred, t) ->
      List
        [ atom "with-accepted-exit-codes"
        ; Predicate_lang.encode Dune_lang.Encoder.int pred
        ; encode t
        ]
    | Dynamic_run (a, xs) ->
      List (atom "run_dynamic" :: program a :: List.map xs ~f:string)
    | Chdir (a, r) -> List [ atom "chdir"; path a; encode r ]
    | Setenv (k, v, r) -> List [ atom "setenv"; string k; string v; encode r ]
    | Redirect_out (outputs, fn, perm, r) ->
      List
        [ atom
            (sprintf "with-%s-to%s"
               (Outputs.to_string outputs)
               (File_perm.suffix perm))
        ; target fn
        ; encode r
        ]
    | Redirect_in (inputs, fn, r) ->
      List
        [ atom (sprintf "with-%s-from" (Inputs.to_string inputs))
        ; path fn
        ; encode r
        ]
    | Ignore (outputs, r) ->
      List [ atom (sprintf "ignore-%s" (Outputs.to_string outputs)); encode r ]
    | Progn l -> List (atom "progn" :: List.map l ~f:encode)
    | Echo xs -> List (atom "echo" :: List.map xs ~f:string)
    | Cat x -> List [ atom "cat"; path x ]
    | Copy (x, y) -> List [ atom "copy"; path x; target y ]
    | Symlink (x, y) -> List [ atom "symlink"; path x; target y ]
    | Hardlink (x, y) -> List [ atom "hardlink"; path x; target y ]
    | Copy_and_add_line_directive (x, y) ->
      List [ atom "copy#"; path x; target y ]
    | System x -> List [ atom "system"; string x ]
    | Bash x -> List [ atom "bash"; string x ]
    | Write_file (x, perm, y) ->
      List [ atom ("write-file" ^ File_perm.suffix perm); target x; string y ]
    | Rename (x, y) -> List [ atom "rename"; target x; target y ]
    | Remove_tree x -> List [ atom "remove-tree"; target x ]
    | Mkdir x -> List [ atom "mkdir"; path x ]
    | Diff { optional; file1; file2; mode = Binary } ->
      assert (not optional);
      List [ atom "cmp"; path file1; target file2 ]
    | Diff { optional = false; file1; file2; mode = _ } ->
      List [ atom "diff"; path file1; target file2 ]
    | Diff { optional = true; file1; file2; mode = _ } ->
      List [ atom "diff?"; path file1; target file2 ]
    | Merge_files_into (srcs, extras, into) ->
      List
        [ atom "merge-files-into"
        ; List (List.map ~f:path srcs)
        ; List (List.map ~f:string extras)
        ; target into
        ]
    | No_infer r -> List [ atom "no-infer"; encode r ]
    | Pipe (outputs, l) ->
      List
        (atom (sprintf "pipe-%s" (Outputs.to_string outputs))
        :: List.map l ~f:encode)
    | Format_dune_file (ver, src, dst) ->
      List
        [ atom "format-dune-file"
        ; Dune_lang.Syntax.Version.encode ver
        ; path src
        ; target dst
        ]
    | Cram script -> List [ atom "cram"; path script ]

  let run prog args = Run (prog, args)

  let chdir path t = Chdir (path, t)

  let setenv var value t = Setenv (var, value, t)

  let with_stdout_to ?(perm = File_perm.Normal) path t =
    Redirect_out (Stdout, path, perm, t)

  let with_stderr_to ?(perm = File_perm.Normal) path t =
    Redirect_out (Stderr, path, perm, t)

  let with_outputs_to ?(perm = File_perm.Normal) path t =
    Redirect_out (Outputs, path, perm, t)

  let with_stdin_from path t = Redirect_in (Stdin, path, t)

  let ignore_stdout t = Ignore (Stdout, t)

  let ignore_stderr t = Ignore (Stderr, t)

  let ignore_outputs t = Ignore (Outputs, t)

  let progn ts = Progn ts

  let echo s = Echo s

  let cat path = Cat path

  let copy a b = Copy (a, b)

  let symlink a b = Symlink (a, b)

  let copy_and_add_line_directive a b = Copy_and_add_line_directive (a, b)

  let system s = System s

  let bash s = Bash s

  let write_file ?(perm = File_perm.Normal) p s = Write_file (p, perm, s)

  let rename a b = Rename (a, b)

  let remove_tree path = Remove_tree path

  let mkdir path = Mkdir path

  let diff ?(optional = false) ?(mode = Diff.Mode.Text) file1 file2 =
    Diff { optional; file1; file2; mode }

  let format_dune_file ~version src dst = Format_dune_file (version, src, dst)
end
