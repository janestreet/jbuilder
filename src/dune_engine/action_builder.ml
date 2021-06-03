open! Stdune
open Import

module T = struct
  type 'a t =
    | Pure : 'a -> 'a t
    | Map : ('a -> 'b) * 'a t -> 'b t
    | Bind : 'a t * ('a -> 'b t) -> 'b t
    | Both : 'a t * 'b t -> ('a * 'b) t
    | Seq : unit t * 'b t -> 'b t
    | All : 'a t list -> 'a list t
    | Map2 : ('a -> 'b -> 'c) * 'a t * 'b t -> 'c t
    | Paths_glob : File_selector.t -> Path.Set.t t
    | Source_tree : Path.t -> Path.Set.t t
    | Dep_on_alias_if_exists : Alias.t -> bool t
    | If_file_exists : Path.t * 'a t * 'a t -> 'a t
    | Contents : Path.t -> string t
    | Lines_of : Path.t -> string list t
    | Dyn_paths : ('a * Path.Set.t) t -> 'a t
    | Dyn_deps : ('a * Dep.Set.t) t -> 'a t
    | Fail : fail -> _ t
    | Memo : 'a memo -> 'a t
    | Deps : Dep.Set.t -> unit t
    | Memo_build : 'a Memo.Build.t -> 'a t
    | Dyn_memo_build : 'a Memo.Build.t t -> 'a t
    | Goal : 'a t -> 'a t
    | Action : Rule.Anonymous_action.t t -> unit t
    | Action_stdout : Rule.Anonymous_action.t t -> string t
    | Push_stack_frame :
        (unit -> User_message.Style.t Pp.t) * (unit -> 'a t)
        -> 'a t

  and 'a memo =
    { name : string
    ; id : 'a Type_eq.Id.t
    ; t : 'a t
    }

  let return x = Pure x

  let map x ~f = Map (f, x)

  let bind x ~f = Bind (x, f)

  let both x y = Both (x, y)

  let all xs = All xs

  let memo_build f = Memo_build f

  module O = struct
    let ( >>> ) a b = Seq (a, b)

    let ( >>= ) t f = Bind (t, f)

    let ( >>| ) t f = Map (f, t)

    let ( and+ ) a b = Both (a, b)

    let ( and* ) a b = Both (a, b)

    let ( let+ ) t f = Map (f, t)

    let ( let* ) t f = Bind (t, f)
  end

  open O

  module List = struct
    let map l ~f = all (List.map l ~f)

    let concat_map l ~f = map l ~f >>| List.concat
  end
end

module Expander = String_with_vars.Make_expander (T)
include T
open O

open struct
  module List = Stdune.List
end

let ignore x = Map (Fun.const (), x)

let map2 x y ~f = Map2 (f, x, y)

let push_stack_frame ~human_readable_description f =
  Push_stack_frame (human_readable_description, f)

let delayed f = Map (f, Pure ())

let all_unit xs =
  let+ (_ : unit list) = all xs in
  ()

let deps d = Deps d

let dep d = Deps (Dep.Set.singleton d)

let dyn_deps x = Dyn_deps x

let path p = Deps (Dep.Set.singleton (Dep.file p))

let paths ps = Deps (Dep.Set.of_files ps)

let path_set ps = Deps (Dep.Set.of_files_set ps)

let paths_matching ~loc:_ dir_glob = Paths_glob dir_glob

let paths_matching_unit ~loc:_ dir_glob = ignore (Paths_glob dir_glob)

let dyn_paths paths =
  Dyn_paths
    (let+ x, paths = paths in
     (x, Path.Set.of_list paths))

let dyn_paths_unit paths =
  Dyn_paths
    (let+ paths = paths in
     ((), Path.Set.of_list paths))

let dyn_path_set paths = Dyn_paths paths

let dyn_path_set_reuse paths =
  Dyn_paths
    (let+ paths = paths in
     (paths, paths))

let env_var s = Deps (Dep.Set.singleton (Dep.env s))

let alias a = dep (Dep.alias a)

let contents p = Contents p

let lines_of p = Lines_of p

let strings p =
  let f x =
    match Scanf.unescaped x with
    | Error () ->
      User_error.raise
        [ Pp.textf "Unable to parse %s" (Path.to_string_maybe_quoted p)
        ; Pp.textf
            "This file must be a list of lines escaped using OCaml's \
             conventions"
        ]
    | Ok s -> s
  in
  Map ((fun l -> List.map l ~f), lines_of p)

let read_sexp p =
  let+ s = contents p in
  Dune_lang.Parser.parse_string s ~fname:(Path.to_string p) ~mode:Single

let if_file_exists p ~then_ ~else_ = If_file_exists (p, then_, else_)

let file_exists p = if_file_exists p ~then_:(return true) ~else_:(return false)

let paths_existing paths =
  all_unit
    (List.map paths ~f:(fun file ->
         if_file_exists file ~then_:(path file) ~else_:(return ())))

let fail x = Fail x

let memoize name t = Memo { name; id = Type_eq.Id.create (); t }

let source_tree ~dir = Source_tree dir

let action t = Action t

let action_stdout t = Action_stdout t

(* CR-someday amokhov: The set of targets is accumulated using information from
   multiple sources by calling [Path.Build.Set.union] and hence occasionally
   duplicate declarations of the very same target go unnoticed. I think such
   redeclarations are not erroneous but are merely redundant; it seems that it
   would be better to rule them out completely.

   Another improvement is to cache [Path.Build.Set.to_list targets] which is
   currently performed multiple times on the very same
   [Action_builder.With_targets.t]. *)
module With_targets = struct
  type nonrec 'a t =
    { build : 'a t
    ; targets : Path.Build.Set.t
    }

  let map_build t ~f = { t with build = f t.build }

  let return x = { build = Pure x; targets = Path.Build.Set.empty }

  let add t ~targets =
    { build = t.build
    ; targets = Path.Build.Set.union t.targets (Path.Build.Set.of_list targets)
    }

  let map { build; targets } ~f = { build = map build ~f; targets }

  let map2 x y ~f =
    { build = Map2 (f, x.build, y.build)
    ; targets = Path.Build.Set.union x.targets y.targets
    }

  let both x y =
    { build = Both (x.build, y.build)
    ; targets = Path.Build.Set.union x.targets y.targets
    }

  let seq x y =
    { build = Seq (x.build, y.build)
    ; targets = Path.Build.Set.union x.targets y.targets
    }

  module O = struct
    let ( >>> ) = seq

    let ( and+ ) = both

    let ( let+ ) a f = map ~f a
  end

  open O

  let all xs =
    match xs with
    | [] -> return []
    | xs ->
      let build, targets =
        List.fold_left xs ~init:([], Path.Build.Set.empty)
          ~f:(fun (xs, set) x ->
            (x.build :: xs, Path.Build.Set.union set x.targets))
      in
      { build = All (List.rev build); targets }

  let write_file_dyn ?(perm = Action.File_perm.Normal) fn s =
    add ~targets:[ fn ]
      (let+ s = s in
       Action.Write_file (fn, perm, s))

  let memoize name t = { build = memoize name t.build; targets = t.targets }
end

let with_targets build ~targets : _ With_targets.t =
  { build; targets = Path.Build.Set.of_list targets }

let with_targets_set build ~targets : _ With_targets.t = { build; targets }

let with_no_targets build : _ With_targets.t =
  { build; targets = Path.Build.Set.empty }

let write_file ?(perm = Action.File_perm.Normal) fn s =
  with_targets ~targets:[ fn ] (return (Action.Write_file (fn, perm, s)))

let write_file_dyn ?(perm = Action.File_perm.Normal) fn s =
  with_targets ~targets:[ fn ]
    (let+ s = s in
     Action.Write_file (fn, perm, s))

let copy ~src ~dst =
  with_targets ~targets:[ dst ] (path src >>> return (Action.Copy (src, dst)))

let copy_and_add_line_directive ~src ~dst =
  with_targets ~targets:[ dst ]
    (path src >>> return (Action.Copy_and_add_line_directive (src, dst)))

let symlink ~src ~dst =
  with_targets ~targets:[ dst ] (path src >>> return (Action.Symlink (src, dst)))

let create_file ?(perm = Action.File_perm.Normal) fn =
  with_targets ~targets:[ fn ]
    (return (Action.Redirect_out (Stdout, fn, perm, Action.empty)))

let progn ts =
  let open With_targets.O in
  let+ actions = With_targets.all ts in
  Action.Progn actions

let goal t = Goal t

let memo_build_join f = Memo_build f |> bind ~f:Fun.id

let dyn_memo_build f = Dyn_memo_build f

let dyn_memo_build_deps t = dyn_deps (dyn_memo_build t)

let dep_on_alias_if_exists t = Dep_on_alias_if_exists t

module Source_tree_map_reduce =
  Source_tree.Dir.Make_map_reduce (T) (Monoid.Exists)

let dep_on_alias_rec name context_name dir =
  let build_dir = Context_name.build_dir context_name in
  let f dir =
    let path = Path.Build.append_source build_dir (Source_tree.Dir.path dir) in
    dep_on_alias_if_exists (Alias.make ~dir:path name)
  in
  Source_tree_map_reduce.map_reduce dir
    ~traverse:Sub_dirs.Status.Set.normal_only ~f

(* Execution *)

type mode =
  | Lazy
  | Eager

let mode = ref Eager

module rec Execution : sig
  val run : 'a t -> ('a * Dep.Facts.t) Memo.Build.t

  val run' : 'a t -> ('a * Rule.facts_or_deps) Memo.Build.t
end = struct
  module Function = struct
    type 'a input = 'a memo

    type 'a output = 'a * Dep.Facts.t

    let name = "exec-memo"

    let id m = m.id

    let to_dyn m = Dyn.String m.name

    let eval m = Execution.run m.t
  end

  module Memo_poly = Memo.Poly (Function)
  open Memo.Build.O

  let register_action_deps deps =
    match !mode with
    | Eager -> Build_system.build_deps deps
    | Lazy ->
      Memo.Build.return (Dep.Map.map deps ~f:(fun () -> Dep.Fact.nothing))

  let register_action_dep_pred g =
    match !mode with
    | Eager ->
      let+ files = Build_system.build_pred g in
      ( Path.Map.keys (Dep.Fact.Files.paths files) |> Path.Set.of_list
      , Dep.Fact.file_selector g files )
    | Lazy ->
      let+ files = Build_system.eval_pred g in
      (files, Dep.Fact.nothing)

  let rec run1 : type a. a t -> (a * Dep.Facts.t) Memo.Build.t = function
    | Pure x -> Memo.Build.return (x, Dep.Map.empty)
    | Map (f, a) ->
      let+ a, deps_a = run1 a in
      (f a, deps_a)
    | Both (a, b) ->
      let+ (a, deps_a), (b, deps_b) =
        Memo.Build.fork_and_join (fun () -> run1 a) (fun () -> run1 b)
      in
      ((a, b), Dep.Facts.union deps_a deps_b)
    | Seq (a, b) ->
      let+ ((), deps_a), (b, deps_b) =
        Memo.Build.fork_and_join (fun () -> run1 a) (fun () -> run1 b)
      in
      (b, Dep.Facts.union deps_a deps_b)
    | Map2 (f, a, b) ->
      let+ (a, deps_a), (b, deps_b) =
        Memo.Build.fork_and_join (fun () -> run1 a) (fun () -> run1 b)
      in
      (f a b, Dep.Facts.union deps_a deps_b)
    | All xs ->
      let+ res = Memo.Build.parallel_map xs ~f:run1 in
      let res, deps = List.split res in
      (res, List.fold_left deps ~init:Dep.Map.empty ~f:Dep.Facts.union)
    | Deps deps ->
      let+ deps = register_action_deps deps in
      ((), deps)
    | Paths_glob g ->
      let+ ps, fact = register_action_dep_pred g in
      (ps, Dep.Map.singleton (Dep.file_selector g) fact)
    | Source_tree dir ->
      let* deps, paths = Dep.Set.source_tree_with_file_set dir in
      let+ deps = register_action_deps deps in
      (paths, deps)
    | Contents p ->
      let+ x = Build_system.read_file p ~f:Io.read_file in
      (x, Dep.Map.empty)
    | Lines_of p ->
      let+ x = Build_system.read_file p ~f:Io.lines_of_file in
      (x, Dep.Map.empty)
    | Dyn_paths t ->
      let* (x, paths), deps_x = run1 t in
      let deps = Dep.Set.of_files_set paths in
      let+ deps = register_action_deps deps in
      (x, Dep.Facts.union deps deps_x)
    | Dyn_deps t ->
      let* (x, deps), deps_x = run1 t in
      let+ deps = register_action_deps deps in
      (x, Dep.Facts.union deps deps_x)
    | Fail { fail } -> fail ()
    | If_file_exists (p, then_, else_) -> (
      Build_system.file_exists p >>= function
      | true -> run1 then_
      | false -> run1 else_)
    | Memo m -> Memo_poly.eval m
    | Memo_build f ->
      let+ f = f in
      (f, Dep.Map.empty)
    | Dyn_memo_build f ->
      let* f, deps = run1 f in
      let+ f = f in
      (f, deps)
    | Bind (t, f) ->
      let* x, deps0 = run1 t in
      let+ r, deps1 = run1 (f x) in
      (r, Dep.Facts.union deps0 deps1)
    | Dep_on_alias_if_exists alias -> (
      let* definition = Build_system.alias_exists alias in
      match definition with
      | false -> Memo.Build.return (false, Dep.Map.empty)
      | true ->
        let deps = Dep.Set.singleton (Dep.alias alias) in
        let+ deps = register_action_deps deps in
        (true, deps))
    | Goal t ->
      let+ a, (_irrelevant_for_goals : Dep.Facts.t) = run1 t in
      (a, Dep.Map.empty)
    | Action t ->
      let* act, facts = run t in
      let+ () = Build_system.execute_action ~observing_facts:facts act in
      ((), Dep.Map.empty)
    | Action_stdout t ->
      let* act, facts = run t in
      let+ s = Build_system.execute_action_stdout ~observing_facts:facts act in
      (s, Dep.Map.empty)
    | Push_stack_frame (human_readable_description, f) ->
      Memo.push_stack_frame ~human_readable_description (fun () -> run1 (f ()))

  and run : type a. a t -> (a * Dep.Facts.t) Memo.Build.t =
   fun t ->
    match !mode with
    | Eager -> run1 t
    | Lazy ->
      let* x, facts = run1 t in
      let deps = Dep.Map.map facts ~f:Stdlib.ignore in
      let* facts = Build_system.build_deps deps in
      Memo.Build.return (x, facts)

  let run' t =
    let+ x, facts = run1 t in
    match !mode with
    | Eager -> (x, Rule.Facts facts)
    | Lazy -> (x, Rule.Deps (Dep.Map.map facts ~f:Stdlib.ignore))
end

include Execution

let set_lazy_mode () = mode := Lazy

let prefix_rules prefix ~f =
  let open Memo.Build.O in
  let* res, rules = Rules.collect f in
  let+ () =
    Rules.produce
      (Rules.map_rules rules ~f:(fun (rule : Rule.t) ->
           let action =
             Memo.lazy_ ~name:"Action_builder.prefix_rules" (fun () ->
                 let* (), facts_or_deps1 = run' prefix
                 and* action, facts_or_deps2 = Memo.Lazy.force rule.action in
                 let+ facts_or_deps =
                   match (facts_or_deps1, facts_or_deps2) with
                   | Facts x, Facts y ->
                     Memo.Build.return (Rule.Facts (Dep.Facts.union x y))
                   | Deps x, Deps y ->
                     Memo.Build.return (Rule.Deps (Dep.Set.union x y))
                   | Facts f, d
                   | d, Facts f
                     when Dep.Map.is_empty f ->
                     Memo.Build.return d
                   | Facts x, Deps y ->
                     let+ y = Build_system.build_deps y in
                     Rule.Facts (Dep.Facts.union x y)
                   | Deps x, Facts y ->
                     let+ x = Build_system.build_deps x in
                     Rule.Facts (Dep.Facts.union x y)
                 in
                 (action, facts_or_deps))
           in
           Rule.set_action rule action))
  in
  res

let add_alias_deps alias ?loc t =
  let open Memo.Build.O in
  Rules.Produce.Alias.add_deps alias ?loc (run' t >>| snd)
