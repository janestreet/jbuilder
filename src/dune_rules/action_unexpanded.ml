open! Dune_engine
open! Stdune
open Import
module Mapper = Action_mapper.Make (Action_dune_lang) (Action_dune_lang)

(* So that we can confortably use both the [Action_builder.O] and [Memo.BUild.O]
   monad at the same time *)
let ( let+! ) = Memo.Build.O.( let+ )

let ( let*! ) = Memo.Build.O.( let* )

let as_in_build_dir ~what ~loc p =
  match Path.as_in_build_dir p with
  | Some p -> p
  | None ->
    User_error.raise ~loc
      [ Pp.textf "%s %s is outside the build directory. This is not allowed."
          what
          (Path.to_string_maybe_quoted p)
      ]

module Action_expander : sig
  (* An applicative to help write action expansion. It is similar to
     [Action_builder.With_targets.t] but with some differences. The differences
     are as follow:

     - it allows to "consume" targets, which is required for interpreting
     [diff?]

     - files that are detected as both targets and dependencies are removed from
     the dependency set

     In addition to this, it embeds an expander for easily expanding
     templates. *)

  include Applicative

  (* Disable targets/dependencies inference detection *)
  val no_infer : 'a t -> 'a t

  val chdir : Path.Build.t -> 'a t -> 'a t

  val set_env : var:string -> value:string t -> (value:string -> 'a) t -> 'a t

  val run :
    'a t -> expander:Expander.t -> 'a Action_builder.With_targets.t Memo.Build.t

  (* String with vars expansion *)
  module E : sig
    val string : String_with_vars.t -> string t

    val strings : String_with_vars.t -> string list t

    (* Evaluate a path in a position of target, such as in [(with-stdout-to
       <target> ...)] *)
    val target : String_with_vars.t -> Path.Build.t t

    (* Evaluate a path in a position of dependency, such as in [(cat <dep>)] *)
    val dep : String_with_vars.t -> Path.t t

    (* Evaluate a path in a position of optional dependency, such as in [(diff
       <dep_if_exists> ...)] *)
    val dep_if_exists : String_with_vars.t -> Path.t t

    (* Evaluate a path that is neither in a position of target or dependency,
       such as in [(chdir <path> ...)] *)
    val path : String_with_vars.t -> Path.t t

    (* Evaluate a path that "consumes" a target, such as in [(diff? ...
       <file>)] *)
    val consume_file : String_with_vars.t -> Path.Build.t t

    val prog_and_args : String_with_vars.t -> (Action.Prog.t * string list) t

    module At_rule_eval_stage : sig
      (* Expansion that happens at the time the rule is constructed rather than
         at the time the rule is being executed. As a result, the result can be
         used immediately. However, percent forms that introduce action
         dependencies are disallowed. *)
      val path : String_with_vars.t -> f:(Path.t -> 'a t) -> 'a t

      val string : String_with_vars.t -> f:(string -> 'a t) -> 'a t
    end
  end
end = struct
  open Action_builder.O

  type deps = Path.Set.t Action_builder.t

  type collector =
    { targets : Path.Build.Set.t
    ; deps : deps
    ; deps_if_exist : deps
    }

  type env =
    { expander : Expander.t
    ; infer : bool
    ; dir : Path.Build.t
    }

  type 'a t = env -> collector -> ('a Action_builder.t * collector) Memo.Build.t

  let return x _env acc = Memo.Build.return (Action_builder.return x, acc)

  let map t ~f env acc =
    let+! b, acc = t env acc in
    (Action_builder.map b ~f, acc)

  let both a b env acc =
    let*! a, acc = a env acc in
    let*! b, acc = b env acc in
    Memo.Build.return (Action_builder.both a b, acc)

  let all =
    let rec loop res l env acc =
      match l with
      | [] ->
        Memo.Build.return
          (Action_builder.map (Action_builder.all res) ~f:List.rev, acc)
      | t :: l ->
        let*! x, acc = t env acc in
        loop (x :: res) l env acc
    in
    fun l env acc -> loop [] l env acc

  let run t ~expander =
    let deps = Action_builder.return Path.Set.empty in
    let acc = { targets = Path.Build.Set.empty; deps; deps_if_exist = deps } in
    let env = { expander; infer = true; dir = Expander.dir expander } in
    Memo.Build.map (t env acc) ~f:(fun (b, acc) ->
        let { targets; deps; deps_if_exist } = acc in
        (* A file can be inferred as both a dependency and a target, for
           instance:

           {[ (progn (copy a b) (copy b c)) ]} *)
        let remove_targets =
          let targets =
            Path.Build.Set.to_list targets |> Path.Set.of_list_map ~f:Path.build
          in
          fun deps -> Path.Set.diff deps targets
        in
        let deps = deps >>| remove_targets in
        let deps_if_exist = deps_if_exist >>| remove_targets in
        let action_builder_path_set_if_exist deps_if_exist =
          Path.Set.fold deps_if_exist ~init:(Action_builder.return ())
            ~f:(fun f acc ->
              acc
              >>> Action_builder.if_file_exists f ~then_:(Action_builder.path f)
                    ~else_:(Action_builder.return ()))
        in
        Action_builder.with_targets_set ~targets
          (let+ () = deps >>= Action_builder.path_set
           and+ () = deps_if_exist >>= action_builder_path_set_if_exist
           and+ res = b in
           res))

  let chdir dir t env acc =
    (* We do not change the directory of the expander to make sure payloads are
       interpreted relative to the directory of the dune file where the action
       is written.

       This is because in:

       {v $ cat a/dune

       (rule (alias default) (action (chdir b (run foo %{dep:p})))) v}

       [p] is treated as relative to "a" rather than "a/b".

       Note However that the expansion of %{dep:p} is done relative to the
       current working directory. i.e. [(run foo %{dep:p})] becomes [(run foo
       ../p)] because of the "chdir ..". *)
    t { env with dir } acc

  let set_env ~var ~value t env acc =
    let*! value, acc = value env acc in
    let value = Action_builder.memoize "env var" value in
    let env =
      { env with
        expander = Expander.set_local_env_var env.expander ~var ~value
      }
    in
    let+! f, acc = t env acc in
    let b =
      let+ f = f
      and+ value = value in
      f ~value
    in
    (b, acc)

  let no_infer t env acc =
    let+! x, _acc = t { env with infer = false } acc in
    (x, acc)

  module O = struct
    let ( let+ ) x f = map x ~f

    let ( and+ ) = both

    let ( >>> ) a b env acc =
      let*! a, acc = a env acc in
      let*! b, acc = b env acc in
      Memo.Build.return (Action_builder.O.( >>> ) a b, acc)
  end

  module E = struct
    let loc = String_with_vars.loc

    (* Override [Expander] to make sure to convert paths to strings relative to
       the current working directory rather than the directory of the expander.
       cf comment for [chdir]. *)
    module Expander = struct
      let expand env ~mode template =
        Action_builder.Expander.expand ~dir:(Path.build env.dir) ~mode template
          ~f:(Expander.expand_pform env.expander)

      let expand_path env sw =
        let+ v = expand env ~mode:Single sw in
        Value.to_path v ~error_loc:(String_with_vars.loc sw)
          ~dir:(Path.build env.dir)

      let expand_string env sw =
        let+ v = expand env ~mode:Single sw in
        Value.to_string v ~dir:(Path.build env.dir)

      let expand_strings env sw =
        let+ v = expand env ~mode:Many sw in
        Value.L.to_strings v ~dir:(Path.build env.dir)

      let artifacts = Expander.artifacts

      let map_exe = Expander.map_exe

      module No_deps = struct
        let expand env ~mode template =
          String_with_vars.expand ~dir:(Path.build env.dir) ~mode template
            ~f:(Expander.No_deps.expand_pform env.expander)

        let expand_path env sw =
          let+! v = expand env ~mode:Single sw in
          Value.to_path v ~error_loc:(String_with_vars.loc sw)
            ~dir:(Path.build env.dir)

        let expand_string env sw =
          let+! v = expand env ~mode:Single sw in
          Value.to_string v ~dir:(Path.build env.dir)
      end
    end

    let string sw env acc =
      Memo.Build.return (Expander.expand_string env sw, acc)

    let strings sw env acc =
      Memo.Build.return (Expander.expand_strings env sw, acc)

    let path sw env acc = Memo.Build.return (Expander.expand_path env sw, acc)

    module At_rule_eval_stage = struct
      let make ~expand sw ~f env acc =
        let*! x = expand env sw in
        f x env acc

      let string sw ~f = make ~expand:Expander.No_deps.expand_string sw ~f

      let path sw ~f = make ~expand:Expander.No_deps.expand_path sw ~f
    end

    let register_dep x ~f env acc =
      Memo.Build.return
        (if not env.infer then
          (x, acc)
        else
          let x = Action_builder.memoize "dep" x in
          ( x
          , { acc with
              deps =
                (let+ x = x
                 and+ set = acc.deps in
                 match f x with
                 | None -> set
                 | Some fn -> Path.Set.add set fn)
            } ))

    let dep sw env acc =
      let fn = Expander.expand_path env sw in
      register_dep fn ~f:Option.some env acc

    let dep_if_exists sw env acc =
      Memo.Build.return
        (let fn = Expander.expand_path env sw in
         if not env.infer then
           (fn, acc)
         else
           let fn = Action_builder.memoize "dep_if_exists" fn in
           ( fn
           , { acc with
               deps_if_exist =
                 (let+ fn = fn
                  and+ set = acc.deps_if_exist in
                  Path.Set.add set fn)
             } ))

    let add_or_remove_target ~what ~f sw env acc =
      if not env.infer then
        Memo.Build.return
          ( (let+ p = Expander.expand_path env sw in
             as_in_build_dir ~what ~loc:(loc sw) p)
          , acc )
      else
        let+! p = Expander.No_deps.expand_path env sw in
        let p = as_in_build_dir p ~what ~loc:(loc sw) in
        (Action_builder.return p, { acc with targets = f acc.targets p })

    let consume_file =
      add_or_remove_target ~what:"File" ~f:Path.Build.Set.remove

    let target = add_or_remove_target ~what:"Target" ~f:Path.Build.Set.add

    let prog_and_args sw env acc =
      let b =
        let dir = Path.build env.dir in
        let loc = loc sw in
        let* prog, args = Expander.expand env sw ~mode:At_least_one in
        let+ prog =
          match prog with
          | Value.Dir p ->
            User_error.raise ~loc
              [ Pp.textf "%s is a directory and cannot be used as an executable"
                  (Path.to_string_maybe_quoted p)
              ]
          | Path p -> Action_builder.return (Ok p)
          | String s -> (
            match Filename.analyze_program_name s with
            | Relative_to_current_dir
            | Absolute ->
              Action_builder.return (Ok (Path.relative dir s))
            | In_path ->
              Action_builder.memo_build
                (Artifacts.Bin.binary ~loc:(Some loc)
                   (Expander.artifacts env.expander)
                   s))
        in
        let prog = Result.map prog ~f:(Expander.map_exe env.expander) in
        let args = Value.L.to_strings ~dir args in
        (prog, args)
      in
      register_dep b env acc ~f:(function
        | Ok p, _ -> Some p
        | Error _, _ -> None)
  end
end

let rec expand (t : Action_dune_lang.t) : Action.t Action_expander.t =
  let module A = Action_expander in
  let module E = Action_expander.E in
  let open Action_expander.O in
  let module O (* [O] for "outcome" *) = Action in
  let expand_run prog args =
    let+ args = A.all (List.map args ~f:E.strings)
    and+ prog, more_args = E.prog_and_args prog in
    let args = List.concat args in
    (prog, more_args @ args)
  in
  match t with
  | Run (prog, args) ->
    let+ prog, args = expand_run prog args in
    O.Run (prog, args)
  | With_accepted_exit_codes (pred, t) ->
    let+ t = expand t in
    O.With_accepted_exit_codes (pred, t)
  | Dynamic_run (prog, args) ->
    let+ prog, args = expand_run prog args in
    O.Dynamic_run (prog, args)
  | Chdir (fn, t) ->
    E.At_rule_eval_stage.path fn ~f:(fun dir ->
        A.chdir
          (as_in_build_dir dir ~loc:(String_with_vars.loc fn) ~what:"Directory")
          (let+ t = expand t in
           O.Chdir (dir, t)))
  | Setenv (var, value, t) ->
    E.At_rule_eval_stage.string var ~f:(fun var ->
        A.set_env ~var ~value:(E.string value)
          (let+ t = expand t in
           fun ~value -> O.Setenv (var, value, t)))
  | Redirect_out (outputs, fn, perm, t) ->
    let+ fn = E.target fn
    and+ t = expand t in
    O.Redirect_out (outputs, fn, perm, t)
  | Redirect_in (inputs, fn, t) ->
    let+ fn = E.dep fn
    and+ t = expand t in
    O.Redirect_in (inputs, fn, t)
  | Ignore (outputs, t) ->
    let+ t = expand t in
    O.Ignore (outputs, t)
  | Progn l ->
    let+ l = A.all (List.map l ~f:expand) in
    O.Progn l
  | Echo xs ->
    let+ l = A.all (List.map xs ~f:E.strings) in
    let l = List.concat l in
    O.Echo l
  | Cat fn ->
    let+ fn = E.dep fn in
    O.Cat fn
  | Copy (x, y) ->
    let+ x = E.dep x
    and+ y = E.target y in
    O.Copy (x, y)
  | Symlink (x, y) ->
    let+ x = E.dep x
    and+ y = E.target y in
    O.Symlink (x, y)
  | Hardlink (x, y) ->
    let+ x = E.dep x
    and+ y = E.target y in
    O.Hardlink (x, y)
  | Copy_and_add_line_directive (x, y) ->
    let+ x = E.dep x
    and+ y = E.target y in
    O.Copy_and_add_line_directive (x, y)
  | System x ->
    let+ x = E.string x in
    O.System x
  | Bash x ->
    let+ x = E.string x in
    O.Bash x
  | Write_file (fn, perm, s) ->
    let+ fn = E.target fn
    and+ s = E.string s in
    O.Write_file (fn, perm, s)
  | Rename (x, _) ->
    (* [Rename] is not part of the syntax so this case is not reachable. The
       reason it is not exposed to the user is because we can't easily decide
       what [x] is. i.e. it might be a file generated by the action and renamed
       immediately, or it could be a dependency. *)
    Code_error.raise ~loc:(String_with_vars.loc x)
      "Rename is not allowed in unexpanded actions" []
  | Remove_tree x ->
    (* Same kind of stuff for [Remove_tree] *)
    Code_error.raise ~loc:(String_with_vars.loc x)
      "Remove_tree is not allowed in unexpanded actions" []
  | Mkdir x ->
    (* This code path should in theory be unreachable too, but we don't delete
       it to remember about the check in in case we expose [mkdir] in the syntax
       one day. *)
    let+ path = E.path x in
    if not (Path.is_managed path) then
      User_error.raise ~loc:(String_with_vars.loc x)
        [ Pp.text
            "(mkdir ...) is not supported for paths outside of the workspace:"
        ; Pp.seq (Pp.verbatim "  ")
            (Dune_lang.pp (List [ Dune_lang.atom "mkdir"; Dpath.encode path ]))
        ];
    O.Mkdir path
  | Diff { optional; file1; file2; mode } ->
    let+ file1 = E.dep_if_exists file1
    and+ file2 =
      if optional then
        E.consume_file file2
      else
        let+ p = E.dep file2 in
        as_in_build_dir p ~loc:(String_with_vars.loc file2) ~what:"File"
    in
    O.Diff { optional; file1; file2; mode }
  | Merge_files_into (sources, extras, target) ->
    let+ sources = A.all (List.map sources ~f:E.dep)
    and+ extras = A.all (List.map extras ~f:E.string)
    and+ target = E.target target in
    O.Merge_files_into (sources, extras, target)
  | No_infer t -> A.no_infer (expand t)
  | Pipe (outputs, l) ->
    let+ l = A.all (List.map l ~f:expand) in
    O.Pipe (outputs, l)
  | Format_dune_file (ver, src, dst) ->
    let+ src = E.dep src
    and+ dst = E.target dst in
    O.Format_dune_file (ver, src, dst)
  | Cram script ->
    let+ script = E.dep script in
    O.Cram script

let pp_path_build target = Pp.text (Dpath.describe_path (Path.build target))

let expand_no_targets t ~loc ~deps:deps_written_by_user ~expander ~what =
  let open Action_builder.O in
  let deps_builder, expander =
    Dep_conf_eval.named ~expander deps_written_by_user
  in
  let expander =
    Expander.set_expanding_what expander (User_action_without_targets { what })
  in
  let* { Action_builder.With_targets.build; targets } =
    Action_builder.memo_build (Action_expander.run (expand t) ~expander)
  in
  if not (Path.Build.Set.is_empty targets) then
    User_error.raise ~loc
      [ Pp.textf
          "%s must not have targets, however I inferred that these files will \
           be created by this action:"
          (String.capitalize what)
      ; Pp.enumerate (Path.Build.Set.to_list targets) ~f:pp_path_build
      ];
  let+ () = deps_builder
  and+ action = build in
  let dir = Path.build (Expander.dir expander) in
  Action.Chdir (dir, action)

let expand t ~loc ~deps:deps_written_by_user ~targets_dir
    ~targets:targets_written_by_user ~expander =
  let open Action_builder.O in
  let deps_builder, expander =
    Dep_conf_eval.named ~expander deps_written_by_user
  in
  let expander =
    match (targets_written_by_user : _ Targets.t) with
    | Infer -> expander
    | Static { targets; multiplicity } ->
      Expander.add_bindings_full expander
        ~bindings:
          (Pform.Map.singleton
             (Var
                (match multiplicity with
                | One -> Target
                | Multiple -> Targets))
             (Expander.Deps.Without
                (Memo.Build.return
                   (Value.L.paths (List.map targets ~f:Path.build)))))
  in
  let expander =
    Expander.set_expanding_what expander (User_action targets_written_by_user)
  in
  let+! { Action_builder.With_targets.build; targets } =
    Action_expander.run (expand t) ~expander
  in
  let targets =
    match (targets_written_by_user : _ Targets.t) with
    | Infer -> targets
    | Static { targets = targets'; multiplicity = _ } ->
      Path.Build.Set.union targets (Path.Build.Set.of_list targets')
  in
  Path.Build.Set.iter targets ~f:(fun target ->
      if Path.Build.( <> ) (Path.Build.parent_exn target) targets_dir then
        User_error.raise ~loc
          [ Pp.text
              "This action has targets in a different directory than the \
               current one, this is not allowed by dune at the moment:"
          ; Pp.enumerate (Path.Build.Set.to_list targets) ~f:pp_path_build
          ]);
  let build =
    let+ () = deps_builder
    and+ action = build in
    let dir = Path.build (Expander.dir expander) in
    Action.Chdir (dir, action)
  in
  Action_builder.with_targets_set ~targets build

(* We re-export [Action_dune_lang] in the end to avoid polluting the inferred
   types in this module with all the various t's *)
include Action_dune_lang
