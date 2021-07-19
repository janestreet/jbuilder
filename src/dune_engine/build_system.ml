open! Stdune
open Import
open Memo.Build.O
module Action_builder = Action_builder0

module Fs : sig
  (** A memoized version of [mkdir] that avoids calling [mkdir] multiple times
      in the same directory. *)
  val mkdir_p : Path.Build.t -> unit Memo.Build.t

  (** If the given path points to the build directory, we call [mkdir_p] on it.
      Otherwise, we assert that the given source or external path exists.

      How can non-build paths appear here? Here are two examples: (i) there are
      rules that copy source files to the build directory, (ii) some rules may
      need to [chdir] to a source directory to run an action there. *)
  val mkdir_p_or_assert_existence : loc:Loc.t -> Path.t -> unit Memo.Build.t
end = struct
  let mkdir_p_memo =
    (* CR-someday amokhov: It's difficult to think about the correctness of this
       memoized function. Right now, we never invalidate it, so if we delete a
       stale build directory, we'll not be able to recreate it later. Perhaps,
       we should create directories as part of running actions that need them.
       That would be less efficient, as we'd call [mkdir] on the same directory
       multiple times, but it would be easier to guarantee correctness.

       Note: if we find a way to reliably invalidate this function, its output
       should continue to have no cutoff because the callers might depend not
       just on the existence of a directory but on its *continuous*
       existence. *)
    Memo.create "mkdir_p"
      ~input:(module Path.Build)
      (fun p ->
        Path.mkdir_p (Path.build p);
        Memo.Build.return ())

  let mkdir_p = Memo.exec mkdir_p_memo

  let mkdir_p_or_assert_existence ~loc path =
    match Path.as_in_build_dir path with
    | Some path -> mkdir_p path
    | None -> (
      Fs_memo.path_exists path >>| function
      | true -> ()
      | false ->
        User_error.raise ~loc
          [ Pp.textf "%S does not exist" (Path.to_string_maybe_quoted path) ])
end

(* [Promoted_to_delete] is used mostly to implement [dune clean]. It is an
   imperfect heuristic, in particular it can go wrong if:

   - the user deletes .to-delete-in-source-tree file

   - the user edits a previously promoted file with the intention of keeping it
   in the source tree, or creates a new file with the same name *)
module Promoted_to_delete : sig
  val add : Path.t -> unit

  val remove : Path.t -> unit

  val mem : Path.t -> bool

  val get_db : unit -> Path.Set.t
end = struct
  module P = Dune_util.Persistent.Make (struct
    type t = Path.Set.t

    let name = "PROMOTED-TO-DELETE"

    let version = 1

    let to_dyn = Path.Set.to_dyn
  end)

  let fn = Path.relative Path.build_dir ".to-delete-in-source-tree"

  (* [db] is used to accumulate promoted files from rules. *)
  let db = lazy (ref (Option.value ~default:Path.Set.empty (P.load fn)))

  let get_db () = !(Lazy.force db)

  let set_db new_db = Lazy.force db := new_db

  let needs_dumping = ref false

  let modify_db f =
    match f (get_db ()) with
    | None -> ()
    | Some new_db ->
      set_db new_db;
      needs_dumping := true

  let add p =
    modify_db (fun db ->
        if Path.Set.mem db p then
          None
        else
          Some (Path.Set.add db p))

  let remove p =
    modify_db (fun db ->
        if Path.Set.mem db p then
          Some (Path.Set.remove db p)
        else
          None)

  let dump () =
    if !needs_dumping && Path.build_dir_exists () then (
      needs_dumping := false;
      get_db () |> P.dump fn
    )

  let mem p = Path.Set.mem !(Lazy.force db) p

  let () = Hooks.End_of_build.always dump
end

let files_in_source_tree_to_delete () = Promoted_to_delete.get_db ()

module Loaded = struct
  type build =
    { allowed_subdirs : Path.Unspecified.w Dir_set.t
    ; rules_produced : Rules.t
    ; rules_here : Rule.t Path.Build.Map.t
    ; aliases : (Loc.t * Rules.Dir_rules.Alias_spec.item) list Alias.Name.Map.t
    }

  type t =
    | Non_build of Path.Set.t
    | Build of build

  let no_rules ~allowed_subdirs =
    Build
      { allowed_subdirs
      ; rules_produced = Rules.empty
      ; rules_here = Path.Build.Map.empty
      ; aliases = Alias.Name.Map.empty
      }
end

module Dir_triage = struct
  type t =
    | Known of Loaded.t
    | Need_step2
end

(* Stores information needed to determine if rule need to be reexecuted. *)
module Trace_db : sig
  module Entry : sig
    type t =
      { rule_digest : Digest.t
      ; dynamic_deps_stages : (Action_exec.Dynamic_dep.Set.t * Digest.t) list
      ; targets_digest : Digest.t
      }
  end

  val get : Path.t -> Entry.t option

  val set : Path.t -> Entry.t -> unit
end = struct
  module Entry = struct
    type t =
      { rule_digest : Digest.t
      ; dynamic_deps_stages : (Action_exec.Dynamic_dep.Set.t * Digest.t) list
      ; targets_digest : Digest.t
      }

    let to_dyn { rule_digest; dynamic_deps_stages; targets_digest } =
      Dyn.Record
        [ ("rule_digest", Digest.to_dyn rule_digest)
        ; ( "dynamic_deps_stages"
          , Dyn.Encoder.list
              (Dyn.Encoder.pair Action_exec.Dynamic_dep.Set.to_dyn Digest.to_dyn)
              dynamic_deps_stages )
        ; ("targets_digest", Digest.to_dyn targets_digest)
        ]
  end

  (* Keyed by the first target of the rule. *)
  type t = Entry.t Path.Table.t

  let file = Path.relative Path.build_dir ".db"

  let to_dyn = Path.Table.to_dyn Entry.to_dyn

  module P = Dune_util.Persistent.Make (struct
    type nonrec t = t

    let name = "INCREMENTAL-DB"

    let version = 4

    let to_dyn = to_dyn
  end)

  let needs_dumping = ref false

  let t =
    lazy
      (match P.load file with
      | Some t -> t
      (* This mutable table is safe: it's only used by [execute_rule_impl] to
         decide whether to rebuild a rule or not; [execute_rule_impl] ensures
         that the targets are produced deterministically. *)
      | None -> Path.Table.create 1024)

  let dump () =
    if !needs_dumping && Path.build_dir_exists () then (
      needs_dumping := false;
      Console.Status_line.set_live_temporarily
        (fun () -> Some (Pp.hbox (Pp.text "Saving build trace db...")))
        (fun () -> P.dump file (Lazy.force t))
    )

  (* CR-someday amokhov: If this happens to be executed after we've cleared the
     status line and printed some text afterwards, [dump] would overwrite that
     text by the "Saving..." message. If this hypothetical scenario turns out to
     be a real problem, we will need to add some synchronisation mechanism to
     prevent clearing the status line too early. *)
  let () = at_exit dump

  let get path =
    let t = Lazy.force t in
    Path.Table.find t path

  let set path e =
    let t = Lazy.force t in
    needs_dumping := true;
    Path.Table.set t path e
end

module Subdir_set = struct
  type t =
    | All
    | These of String.Set.t

  let to_dir_set = function
    | All -> Dir_set.universal
    | These s ->
      String.Set.fold s ~init:Dir_set.empty ~f:(fun path acc ->
          let path = Path.Local.of_string path in
          Dir_set.union acc (Dir_set.singleton path))

  let of_dir_set d =
    match Dir_set.toplevel_subdirs d with
    | Infinite -> All
    | Finite s -> These s

  let of_list l = These (String.Set.of_list l)

  let empty = These String.Set.empty

  let mem t dir =
    match t with
    | All -> true
    | These t -> String.Set.mem t dir

  let union a b =
    match (a, b) with
    | All, _
    | _, All ->
      All
    | These a, These b -> These (String.Set.union a b)

  let union_all = List.fold_left ~init:empty ~f:union
end

type extra_sub_directories_to_keep = Subdir_set.t

module Rule_fn = struct
  let loc_decl = Fdecl.create Dyn.Encoder.opaque

  let loc () = Fdecl.get loc_decl ()
end

module Context_or_install = struct
  type t =
    | Install of Context_name.t
    | Context of Context_name.t

  let to_dyn = function
    | Install ctx -> Dyn.List [ Dyn.String "install"; Context_name.to_dyn ctx ]
    | Context s -> Context_name.to_dyn s
end

module Error = struct
  module Id = Id.Make ()

  type t =
    { exn : Exn_with_backtrace.t
    ; id : Id.t
    }

  let id t = Id.to_int t.id

  let extract_dir annot =
    Process.With_directory_annot.check annot
      (fun dir -> Some dir)
      (fun () -> None)

  let extract_promote annot =
    Promotion.Annot.check annot (fun promote -> Some promote) (fun () -> None)

  let promotion t =
    let e =
      match t.exn.exn with
      | Memo.Error.E e -> Memo.Error.get e
      | e -> e
    in
    match e with
    | User_error.E (_, annots) -> List.find_map annots ~f:extract_promote
    | _ -> None

  let extract_compound_error annot =
    Compound_user_error.check annot Option.some (fun () -> None)

  let info (t : t) =
    let e =
      match t.exn.exn with
      | Memo.Error.E e -> Memo.Error.get e
      | e -> e
    in
    match e with
    | User_error.E (msg, annots) -> (
      let dir = List.find_map annots ~f:extract_dir in
      match List.find_map annots ~f:extract_compound_error with
      | None -> (msg, [], dir)
      | Some { main; related } -> (main, related, dir))
    | e ->
      (* CR-someday jeremiedimino: Use [Report_error.get_user_message] here. *)
      (User_message.make [ Pp.text (Printexc.to_string e) ], [], None)
end

module type Rule_generator = sig
  val gen_rules :
       Context_or_install.t
    -> dir:Path.Build.t
    -> string list
    -> (extra_sub_directories_to_keep * Rules.t) option Memo.Build.t

  val global_rules : Rules.t Memo.Lazy.t
end

module Handler = struct
  type event =
    | Start
    | Finish
    | Fail
    | Interrupt

  type error =
    | Add of Error.t
    | Remove of Error.t

  type t =
    { error : error list -> unit Fiber.t
    ; build_progress : complete:int -> remaining:int -> unit Fiber.t
    ; build_event : event -> unit Fiber.t
    }

  let report_progress t ~rule_done ~rule_total =
    t.build_progress ~complete:rule_done ~remaining:(rule_total - rule_done)

  let last_event : event option ref = ref None

  let report_build_event t evt =
    last_event := Some evt;
    t.build_event evt

  let do_nothing =
    { error = (fun _ -> Fiber.return ())
    ; build_progress = (fun ~complete:_ ~remaining:_ -> Fiber.return ())
    ; build_event = (fun _ -> Fiber.return ())
    }

  let create ~error ~build_progress ~build_event =
    { error; build_progress; build_event }
end

type t =
  { contexts : Build_context.t Context_name.Map.t Memo.Lazy.t
  ; rule_generator : (module Rule_generator)
  ; sandboxing_preference : Sandbox_mode.t list
  ; mutable rule_done : int
  ; mutable rule_total : int
  ; mutable errors : Error.t list
  ; handler : Handler.t
  ; promote_source :
         ?chmod:(int -> int)
      -> src:Path.Build.t
      -> dst:Path.Source.t
      -> Build_context.t option
      -> unit Fiber.t
  ; locks : (Path.t, Fiber.Mutex.t) Table.t
  ; build_mutex : Fiber.Mutex.t
  ; stats : Dune_stats.t option
  ; cache_config : Dune_cache.Config.t
  ; cache_debug_flags : Cache_debug_flags.t
  ; implicit_default_alias :
      Path.Build.t -> unit Action_builder.t option Memo.Build.t
  }

let t = Fdecl.create Dyn.Encoder.opaque

let set x = Fdecl.set t x

let t () = Fdecl.get t

let errors () = (t ()).errors

let pp_path p =
  Path.drop_optional_build_context p
  |> Path.to_string_maybe_quoted |> Pp.verbatim

let pp_paths set = Pp.enumerate (Path.Set.to_list set) ~f:pp_path

let get_dir_triage t ~dir =
  match Dpath.analyse_dir dir with
  | Source dir ->
    let+ files = Source_tree.files_of dir in
    Dir_triage.Known (Non_build (Path.set_of_source_paths files))
  | External _ ->
    Memo.Build.return
    @@ Dir_triage.Known
         (Non_build
            (match Path.readdir_unsorted dir with
            | Error Unix.ENOENT -> Path.Set.empty
            | Error m ->
              User_warning.emit
                [ Pp.textf "Unable to read %s" (Path.to_string_maybe_quoted dir)
                ; Pp.textf "Reason: %s" (Unix.error_message m)
                ];
              Path.Set.empty
            | Ok filenames -> Path.Set.of_listing ~dir ~filenames))
  | Build (Regular Root) ->
    let+ contexts = Memo.Lazy.force t.contexts in
    let allowed_subdirs =
      Subdir_set.to_dir_set
        (Subdir_set.of_list
           (([ Dpath.Build.anonymous_actions_dir; Dpath.Build.install_dir ]
            |> List.map ~f:Path.Build.basename)
           @ (Context_name.Map.keys contexts
             |> List.map ~f:Context_name.to_string)))
    in
    Dir_triage.Known (Loaded.no_rules ~allowed_subdirs)
  | Build (Install Root) ->
    let+ contexts = Memo.Lazy.force t.contexts in
    let allowed_subdirs =
      Context_name.Map.keys contexts
      |> List.map ~f:Context_name.to_string
      |> Subdir_set.of_list |> Subdir_set.to_dir_set
    in
    Dir_triage.Known (Loaded.no_rules ~allowed_subdirs)
  | Build (Anonymous_action p) ->
    let build_dir = Dpath.Target_dir.build_dir p in
    Code_error.raise "Called get_dir_triage on an anonymous action directory"
      [ ("dir", Path.Build.to_dyn build_dir) ]
  | Build (Invalid _) ->
    Memo.Build.return
    @@ Dir_triage.Known (Loaded.no_rules ~allowed_subdirs:Dir_set.empty)
  | Build (Install (With_context _))
  | Build (Regular (With_context _)) ->
    Memo.Build.return @@ Dir_triage.Need_step2

let describe_rule (rule : Rule.t) =
  match rule.info with
  | From_dune_file { start; _ } ->
    start.pos_fname ^ ":" ^ string_of_int start.pos_lnum
  | Internal -> "<internal location>"
  | Source_file_copy _ -> "file present in source tree"

let report_rule_src_dir_conflict dir fn (rule : Rule.t) =
  let loc =
    match rule.info with
    | From_dune_file loc -> loc
    | Internal
    | Source_file_copy _ ->
      let dir =
        match Path.Build.drop_build_context dir with
        | None -> Path.build dir
        | Some s -> Path.source s
      in
      Loc.in_dir dir
  in
  User_error.raise ~loc
    [ Pp.textf "Rule has a target %s" (Path.Build.to_string_maybe_quoted fn)
    ; Pp.textf "This conflicts with a source directory in the same directory"
    ]

let report_rule_conflict fn (rule' : Rule.t) (rule : Rule.t) =
  let fn = Path.build fn in
  User_error.raise
    [ Pp.textf "Multiple rules generated for %s:"
        (Path.to_string_maybe_quoted fn)
    ; Pp.textf "- %s" (describe_rule rule')
    ; Pp.textf "- %s" (describe_rule rule)
    ]
    ~hints:
      (match (rule.info, rule'.info) with
      | Source_file_copy _, _
      | _, Source_file_copy _ ->
        [ Pp.textf "rm -f %s"
            (Path.to_string_maybe_quoted (Path.drop_optional_build_context fn))
        ]
      | _ -> [])

(* This contains the targets of the actions that are being executed. On exit, we
   need to delete them as they might contain garbage. *)
let pending_targets = ref Path.Build.Set.empty

let () = Hooks.End_of_build.always Metrics.reset

let () =
  Hooks.End_of_build.always (fun () ->
      let fns = !pending_targets in
      pending_targets := Path.Build.Set.empty;
      Path.Build.Set.iter fns ~f:(fun p -> Path.unlink_no_err (Path.build p)))

let compute_target_digests targets =
  match
    List.map (Path.Build.Set.to_list targets) ~f:(fun target ->
        (target, Cached_digest.build_file target))
  with
  | l -> Some l
  | exception (Unix.Unix_error _ | Sys_error _) -> None

let compute_target_digests_or_raise_error exec_params ~loc targets =
  let remove_write_permissions =
    (* Remove write permissions on targets. A first theoretical reason is that
       the build process should be a computational graph and targets should not
       change state once built. A very practical reason is that enabling the
       cache will remove write permission because of hardlink sharing anyway, so
       always removing them enables to catch mistakes earlier. *)
    (* FIXME: searching the dune version for each single target seems way
       suboptimal. This information could probably be stored in rules
       directly. *)
    if Path.Build.Set.is_empty targets then
      false
    else
      Execution_parameters.should_remove_write_permissions_on_generated_files
        exec_params
  in
  let good, missing, errors =
    Path.Build.Set.fold targets ~init:([], [], [])
      ~f:(fun target (good, missing, errors) ->
        match Cached_digest.refresh ~remove_write_permissions target with
        | Ok digest -> ((target, digest) :: good, missing, errors)
        | No_such_file -> (good, target :: missing, errors)
        | Error exn -> (good, missing, (target, exn) :: errors))
  in
  match (missing, errors) with
  | [], [] -> List.rev good
  | missing, errors ->
    User_error.raise ~loc
      ((match missing with
       | [] -> []
       | _ ->
         [ Pp.textf "Rule failed to generate the following targets:"
         ; pp_paths (Path.Set.of_list (List.map ~f:Path.build missing))
         ])
      @
      match errors with
      | [] -> []
      | _ ->
        [ Pp.textf "Error trying to read targets after a rule was run:"
        ; Pp.enumerate (List.rev errors) ~f:(fun (path, exn) ->
              let path = Path.build path in
              let expected_syscall_path = Path.to_string path in
              Pp.concat ~sep:(Pp.verbatim ": ")
                (pp_path path
                ::
                (match exn with
                | Unix.Unix_error (error, syscall, p) ->
                  [ (if String.equal expected_syscall_path p then
                      Pp.verbatim syscall
                    else
                      Pp.concat
                        [ Pp.verbatim syscall
                        ; Pp.verbatim " "
                        ; Pp.verbatim (String.maybe_quoted p)
                        ])
                  ; Pp.text (Unix.error_message error)
                  ]
                | Sys_error msg ->
                  [ Pp.verbatim
                      (String.drop_prefix_if_exists
                         ~prefix:(expected_syscall_path ^ ": ")
                         msg)
                  ]
                | exn -> [ Pp.verbatim (Printexc.to_string exn) ])))
        ])

let sandbox_dir = Path.Build.relative Path.Build.root ".sandbox"

let rec with_locks t mutexes ~f =
  match mutexes with
  | [] -> f ()
  | m :: mutexes ->
    Fiber.Mutex.with_lock
      (Table.find_or_add t.locks m ~f:(fun _ -> Fiber.Mutex.create ()))
      (fun () -> with_locks t mutexes ~f)

let remove_old_artifacts ~dir ~rules_here ~(subdirs_to_keep : Subdir_set.t) =
  match Path.Untracked.readdir_unsorted_with_kinds (Path.build dir) with
  | exception _ -> ()
  | Error _ -> ()
  | Ok files ->
    List.iter files ~f:(fun (fn, kind) ->
        let path = Path.Build.relative dir fn in
        let path_is_a_target = Path.Build.Map.mem rules_here path in
        if not path_is_a_target then
          match kind with
          | Unix.S_DIR -> (
            match subdirs_to_keep with
            | All -> ()
            | These set ->
              if not (String.Set.mem set fn) then Path.rm_rf (Path.build path))
          | _ -> Path.unlink (Path.build path))

(* We don't remove files in there as we don't know upfront if they are stale or
   not. *)
let remove_old_sub_dirs_in_anonymous_actions_dir ~dir
    ~(subdirs_to_keep : Subdir_set.t) =
  match Path.Untracked.readdir_unsorted_with_kinds (Path.build dir) with
  | exception _ -> ()
  | Error _ -> ()
  | Ok files ->
    List.iter files ~f:(fun (fn, kind) ->
        let path = Path.Build.relative dir fn in
        match kind with
        | Unix.S_DIR -> (
          match subdirs_to_keep with
          | All -> ()
          | These set ->
            if not (String.Set.mem set fn) then Path.rm_rf (Path.build path))
        | _ -> ())

let no_rule_found t ~loc fn =
  let+ contexts = Memo.Lazy.force t.contexts in
  let fail fn ~loc =
    User_error.raise ?loc
      [ Pp.textf "No rule found for %s" (Dpath.describe_target fn) ]
  in
  let hints ctx =
    let candidates =
      Context_name.Map.keys contexts |> List.map ~f:Context_name.to_string
    in
    User_message.did_you_mean (Context_name.to_string ctx) ~candidates
  in
  match Dpath.analyse_target fn with
  | Other _ -> fail fn ~loc
  | Regular (ctx, _) ->
    if Context_name.Map.mem contexts ctx then
      fail fn ~loc
    else
      User_error.raise
        [ Pp.textf "Trying to build %s but build context %s doesn't exist."
            (Path.Build.to_string_maybe_quoted fn)
            (Context_name.to_string ctx)
        ]
        ~hints:(hints ctx)
  | Install (ctx, _) ->
    if Context_name.Map.mem contexts ctx then
      fail fn ~loc
    else
      User_error.raise
        [ Pp.textf
            "Trying to build %s for install but build context %s doesn't exist."
            (Path.Build.to_string_maybe_quoted fn)
            (Context_name.to_string ctx)
        ]
        ~hints:(hints ctx)
  | Alias (ctx, fn') ->
    if Context_name.Map.mem contexts ctx then
      fail fn ~loc
    else
      let fn =
        Path.append_source (Path.build (Context_name.build_dir ctx)) fn'
      in
      User_error.raise
        [ Pp.textf
            "Trying to build alias %s but build context %s doesn't exist."
            (Path.to_string_maybe_quoted fn)
            (Context_name.to_string ctx)
        ]
        ~hints:(hints ctx)
  | Anonymous_action _ ->
    (* We never lookup such actions by target name, so this should be
       unreachable *)
    Code_error.raise ?loc "Build_system.no_rule_found got anonymous action path"
      [ ("fn", Path.Build.to_dyn fn) ]

(* +-------------------- Adding rules to the system --------------------+ *)

let source_file_digest path =
  Fs_memo.path_exists path >>= function
  | true ->
    let+ d = Fs_memo.file_digest path in
    d
  | false ->
    let+ loc = Rule_fn.loc () in
    User_error.raise ?loc
      [ Pp.textf "File unavailable: %s" (Path.to_string_maybe_quoted path) ]

let eval_source_file :
    type a. a Action_builder.eval_mode -> Path.t -> a Memo.Build.t =
 fun mode path ->
  match mode with
  | Lazy -> Memo.Build.return ()
  | Eager ->
    let+ d = source_file_digest path in
    Dep.Fact.file path d

module rec Load_rules : sig
  val load_dir : dir:Path.t -> Loaded.t Memo.Build.t

  val file_exists : Path.t -> bool Memo.Build.t

  val targets_of : dir:Path.t -> Path.Set.t Memo.Build.t

  val lookup_alias :
       Alias.t
    -> (Loc.t * Rules.Dir_rules.Alias_spec.item) list option Memo.Build.t

  val alias_exists : Alias.t -> bool Memo.Build.t
end = struct
  open Load_rules

  let create_copy_rules ~ctx_dir ~non_target_source_files =
    Path.Source.Set.to_list_map non_target_source_files ~f:(fun path ->
        let ctx_path = Path.Build.append_source ctx_dir path in
        let build =
          Action_builder.of_thunk
            { f =
                (fun mode ->
                  let path = Path.source path in
                  let+ fact = eval_source_file mode path in
                  ( { Action.Full.action = Action.copy path ctx_path
                    ; env = Env.empty
                    ; locks = []
                    ; can_go_in_shared_cache = true
                    }
                  , Dep.Map.singleton (Dep.file path) fact ))
            }
        in
        Rule.make
        (* There's an [assert false] in [prepare_managed_paths] that blows up if
           we try to sandbox this. *)
          ~sandbox:Sandbox_config.no_sandboxing ~context:None
          ~info:(Source_file_copy path)
          ~targets:(Path.Build.Set.singleton ctx_path)
          build)

  let compile_rules ~dir ~source_dirs rules =
    List.concat_map rules ~f:(fun rule ->
        assert (Path.Build.( = ) dir rule.Rule.dir);
        Path.Build.Set.to_list_map rule.targets ~f:(fun target ->
            if String.Set.mem source_dirs (Path.Build.basename target) then
              report_rule_src_dir_conflict dir target rule
            else
              (target, rule)))
    |> Path.Build.Map.of_list_reducei ~f:report_rule_conflict

  (* Here we are doing a O(log |S|) lookup in a set S of files in the build
     directory [dir]. We could memoize these lookups, but it doesn't seem to be
     worth it, since we're unlikely to perform exactly the same lookup many
     times. As far as I can tell, each lookup will be done twice: when computing
     static dependencies of a [Action_builder.t] with
     [Action_builder.static_deps] and when executing the very same
     [Action_builder.t] with [Action_builder.exec] -- the results of both
     [Action_builder.static_deps] and [Action_builder.exec] are cached. *)
  let file_exists fn =
    load_dir ~dir:(Path.parent_exn fn) >>| function
    | Non_build targets -> Path.Set.mem targets fn
    | Build { rules_here; _ } -> (
      match Path.as_in_build_dir fn with
      | None -> false
      | Some fn -> Path.Build.Map.mem rules_here fn)

  let targets_of ~dir =
    load_dir ~dir >>| function
    | Non_build targets -> targets
    | Build { rules_here; _ } ->
      Path.Build.Map.keys rules_here |> Path.Set.of_list_map ~f:Path.build

  let lookup_alias alias =
    load_dir ~dir:(Path.build (Alias.dir alias)) >>| function
    | Non_build _ ->
      Code_error.raise "Alias in a non-build dir"
        [ ("alias", Alias.to_dyn alias) ]
    | Build { aliases; _ } -> Alias.Name.Map.find aliases (Alias.name alias)

  let alias_exists alias =
    lookup_alias alias >>| function
    | None -> false
    | Some _ -> true

  let compute_alias_expansions t ~(collected : Rules.Dir_rules.ready) ~dir =
    let aliases = collected.aliases in
    let+ aliases =
      if Alias.Name.Map.mem aliases Alias.Name.default then
        Memo.Build.return aliases
      else
        t.implicit_default_alias dir >>| function
        | None -> aliases
        | Some expansion ->
          Alias.Name.Map.set aliases Alias.Name.default
            { expansions =
                Appendable_list.singleton
                  (Loc.none, Rules.Dir_rules.Alias_spec.Deps expansion)
            }
    in
    Alias.Name.Map.map aliases
      ~f:(fun { Rules.Dir_rules.Alias_spec.expansions } ->
        Appendable_list.to_list expansions)

  let filter_out_fallback_rules ~to_copy rules =
    List.filter rules ~f:(fun (rule : Rule.t) ->
        match rule.mode with
        | Standard
        | Promote _
        | Ignore_source_files ->
          true
        | Fallback ->
          let source_files_for_targtes =
            (* All targets are in [dir] and we know it correspond to a directory
               of a build context since there are source files to copy, so this
               call can't fail. *)
            Path.Build.Set.to_list rule.targets
            |> Path.Source.Set.of_list_map ~f:Path.Build.drop_build_context_exn
          in
          if Path.Source.Set.is_subset source_files_for_targtes ~of_:to_copy
          then
            (* All targets are present *)
            false
          else if
            Path.Source.Set.is_empty
              (Path.Source.Set.inter source_files_for_targtes to_copy)
          then
            (* No target is present *)
            true
          else
            let absent_targets =
              Path.Source.Set.diff source_files_for_targtes to_copy
            in
            let present_targets =
              Path.Source.Set.diff source_files_for_targtes absent_targets
            in
            User_error.raise ~loc:(Rule.loc rule)
              [ Pp.text
                  "Some of the targets of this fallback rule are present in \
                   the source tree, and some are not. This is not allowed. \
                   Either none of the targets must be present in the source \
                   tree, either they must all be."
              ; Pp.nop
              ; Pp.text "The following targets are present:"
              ; pp_paths (Path.set_of_source_paths present_targets)
              ; Pp.nop
              ; Pp.text "The following targets are not:"
              ; pp_paths (Path.set_of_source_paths absent_targets)
              ])

  (** A directory is only allowed to be generated if its parent knows about it.
      This restriction is necessary to prevent stale artifact deletion from
      removing that directory.

      This module encodes that restriction. *)
  module Generated_directory_restrictions : sig
    type restriction =
      | Unrestricted
      | Restricted of Path.Unspecified.w Dir_set.t Memo.Lazy.t

    (** Used by the child to ask about the restrictions placed by the parent. *)
    val allowed_by_parent : dir:Path.Build.t -> restriction Memo.Build.t
  end = struct
    type restriction =
      | Unrestricted
      | Restricted of Path.Unspecified.w Dir_set.t Memo.Lazy.t

    let corresponding_source_dir ~dir =
      match Dpath.analyse_target dir with
      | Install _
      | Alias _
      | Anonymous_action _
      | Other _ ->
        Memo.Build.return None
      | Regular (_ctx, sub_dir) -> Source_tree.find_dir sub_dir

    let source_subdirs_of_build_dir ~dir =
      corresponding_source_dir ~dir >>| function
      | None -> String.Set.empty
      | Some dir -> Source_tree.Dir.sub_dir_names dir

    let allowed_dirs ~dir ~subdir : restriction Memo.Build.t =
      let+ subdirs = source_subdirs_of_build_dir ~dir in
      if String.Set.mem subdirs subdir then
        Unrestricted
      else
        Restricted
          (Memo.Lazy.create (fun () ->
               load_dir ~dir:(Path.build dir) >>| function
               | Non_build _ -> Dir_set.just_the_root
               | Build { allowed_subdirs; _ } ->
                 Dir_set.descend allowed_subdirs subdir))

    let allowed_by_parent ~dir =
      allowed_dirs
        ~dir:(Path.Build.parent_exn dir)
        ~subdir:(Path.Build.basename dir)
  end

  (* TODO: Delete this step after users of dune <2.8 are sufficiently rare. This
     step is sketchy because it's using the [Promoted_to_delete] database and
     that can get out of date (see a comment on [Promoted_to_delete]), so we
     should not widen the scope of it too much. *)
  let delete_stale_dot_merlin_file ~dir ~source_files_to_ignore =
    (* If a [.merlin] file is present in the [Promoted_to_delete] set but not in
       the [Source_files_to_ignore] that means the rule that ordered its
       promotion is no more valid. This would happen when upgrading to Dune 2.8
       from ealier version without and building uncleaned projects. We delete
       these leftover files here. *)
    let merlin_file = ".merlin" in
    let source_dir = Path.Build.drop_build_context_exn dir in
    let merlin_in_src = Path.Source.(relative source_dir merlin_file) in
    let source_files_to_ignore =
      if
        Promoted_to_delete.mem (Path.source merlin_in_src)
        && not (Path.Source.Set.mem source_files_to_ignore merlin_in_src)
      then (
        let path = Path.source merlin_in_src in
        Log.info
          [ Pp.textf "Deleting left-over Merlin file %s.\n"
              (Path.to_string path)
          ];
        (* We remove the file from the promoted database *)
        Promoted_to_delete.remove path;
        Path.unlink_no_err path;
        (* We need to keep ignoring the .merlin file for that build or Dune will
           attempt to copy it and fail because it has been deleted *)
        Path.Source.Set.add source_files_to_ignore merlin_in_src
      ) else
        source_files_to_ignore
    in
    source_files_to_ignore

  let load_dir_step2_exn t ~dir =
    let context_name, sub_dir =
      match Dpath.analyse_path dir with
      | Build (Install (ctx, path)) -> (Context_or_install.Install ctx, path)
      | Build (Regular (ctx, path)) -> (Context_or_install.Context ctx, path)
      | Build (Alias _)
      | Build (Anonymous_action _)
      | Build (Other _)
      | Source _
      | External _ ->
        Code_error.raise "[load_dir_step2_exn] was called on a strange path"
          [ ("path", Path.to_dyn dir) ]
    in
    (* the above check makes this safe *)
    let dir = Path.as_in_build_dir_exn dir in
    let sub_dir_components = Path.Source.explode sub_dir in
    (* Load all the rules *)
    let (module RG : Rule_generator) = t.rule_generator in
    let* extra_subdirs_to_keep, rules_produced =
      RG.gen_rules context_name ~dir sub_dir_components >>| function
      | None ->
        Code_error.raise "[gen_rules] did not specify rules for the context"
          [ ("context_name", Context_or_install.to_dyn context_name) ]
      | Some x -> x
    and* global_rules = Memo.Lazy.force RG.global_rules in
    let rules =
      let dir = Path.build dir in
      Rules.Dir_rules.union
        (Rules.find rules_produced dir)
        (Rules.find global_rules dir)
    in
    let collected = Rules.Dir_rules.consume rules in
    let rules = collected.rules in
    let* aliases =
      match context_name with
      | Context _ -> compute_alias_expansions t ~collected ~dir
      | Install _ ->
        (* There are no aliases in the [_build/install] directory *)
        Memo.Build.return Alias.Name.Map.empty
    and* source_tree_dir =
      match context_name with
      | Install _ -> Memo.Build.return None
      | Context _ -> Source_tree.find_dir sub_dir
    in
    (* Compute the set of targets and the set of source files that must not be
       copied *)
    let source_files_to_ignore =
      List.fold_left rules ~init:Path.Build.Set.empty
        ~f:(fun acc_ignored { Rule.targets; mode; _ } ->
          match mode with
          | Promote { only = None; _ }
          | Ignore_source_files ->
            Path.Build.Set.union targets acc_ignored
          | Promote { only = Some pred; _ } ->
            let to_ignore =
              Path.Build.Set.filter targets ~f:(fun target ->
                  Predicate_lang.Glob.exec pred
                    (Path.reach (Path.build target) ~from:(Path.build dir))
                    ~standard:Predicate_lang.any)
            in
            Path.Build.Set.union to_ignore acc_ignored
          | _ -> acc_ignored)
    in
    let source_files_to_ignore =
      Path.Build.Set.to_list source_files_to_ignore
      |> Path.Source.Set.of_list_map ~f:Path.Build.drop_build_context_exn
    in
    let source_files_to_ignore =
      delete_stale_dot_merlin_file ~dir ~source_files_to_ignore
    in
    (* Take into account the source files *)
    let to_copy, source_dirs =
      match context_name with
      | Install _ -> (None, String.Set.empty)
      | Context context_name ->
        let files, subdirs =
          match source_tree_dir with
          | None -> (Path.Source.Set.empty, String.Set.empty)
          | Some dir ->
            (Source_tree.Dir.file_paths dir, Source_tree.Dir.sub_dir_names dir)
        in
        let files = Path.Source.Set.diff files source_files_to_ignore in
        if Path.Source.Set.is_empty files then
          (None, subdirs)
        else
          let ctx_path = Context_name.build_dir context_name in
          (Some (ctx_path, files), subdirs)
    in
    let subdirs_to_keep =
      match extra_subdirs_to_keep with
      | All -> Subdir_set.All
      | These set -> These (String.Set.union source_dirs set)
    in
    (* Filter out fallback rules *)
    let rules =
      match to_copy with
      | None ->
        (* If there are no source files to copy, fallback rules are
           automatically kept *)
        rules
      | Some (_, to_copy) -> filter_out_fallback_rules ~to_copy rules
    in
    (* Compile the rules and cleanup stale artifacts *)
    let rules =
      (match to_copy with
      | None -> []
      | Some (ctx_dir, source_files) ->
        create_copy_rules ~ctx_dir ~non_target_source_files:source_files)
      @ rules
    in
    let rules_here = compile_rules ~dir ~source_dirs rules in
    let* allowed_by_parent =
      match (context_name, sub_dir_components) with
      | Context _, [ ".dune" ] ->
        (* GROSS HACK: this is to avoid a cycle as the rules for all directories
           force the generation of ".dune/configurator". We need a better way to
           deal with such cases. *)
        Memo.Build.return Generated_directory_restrictions.Unrestricted
      | _ -> Generated_directory_restrictions.allowed_by_parent ~dir
    in
    let* () =
      match allowed_by_parent with
      | Unrestricted -> Memo.Build.return ()
      | Restricted restriction -> (
        match Path.Build.Map.find (Rules.to_map rules_produced) dir with
        | None -> Memo.Build.return ()
        | Some rules ->
          let+ restriction = Memo.Lazy.force restriction in
          if not (Dir_set.here restriction) then
            Code_error.raise
              "Generated rules in a directory not allowed by the parent"
              [ ("dir", Path.Build.to_dyn dir)
              ; ("rules", Rules.Dir_rules.to_dyn rules)
              ])
    in
    let rules_generated_in =
      Rules.to_map rules_produced
      |> Path.Build.Map.foldi ~init:Dir_set.empty ~f:(fun p _ acc ->
             match Path.Local_gen.descendant ~of_:dir p with
             | None -> acc
             | Some p -> Dir_set.union acc (Dir_set.singleton p))
    in
    let* allowed_granddescendants_of_parent =
      match allowed_by_parent with
      | Unrestricted ->
        (* In this case the parent isn't going to be able to create any
           generated granddescendant directories. (rules that attempt to do so
           may run into the [allowed_by_parent] check or will be simply
           ignored) *)
        Memo.Build.return Dir_set.empty
      | Restricted restriction -> Memo.Lazy.force restriction
    in
    let descendants_to_keep =
      Dir_set.union_all
        [ rules_generated_in
        ; Subdir_set.to_dir_set subdirs_to_keep
        ; allowed_granddescendants_of_parent
        ]
    in
    let subdirs_to_keep = Subdir_set.of_dir_set descendants_to_keep in
    remove_old_artifacts ~dir ~rules_here ~subdirs_to_keep;
    remove_old_sub_dirs_in_anonymous_actions_dir
      ~dir:
        (Path.Build.append_local Dpath.Build.anonymous_actions_dir
           (Path.Build.local dir))
      ~subdirs_to_keep;
    Memo.Build.return
      (Loaded.Build
         { allowed_subdirs = descendants_to_keep
         ; rules_produced
         ; rules_here
         ; aliases
         })

  let load_dir_impl t ~dir : Loaded.t Memo.Build.t =
    get_dir_triage t ~dir >>= function
    | Known l -> Memo.Build.return l
    | Need_step2 -> load_dir_step2_exn t ~dir

  let load_dir =
    let load_dir_impl dir = load_dir_impl (t ()) ~dir in
    let memo = Memo.create "load-dir" ~input:(module Path) load_dir_impl in
    fun ~dir -> Memo.exec memo dir
end

open Load_rules

let load_dir_and_get_buildable_targets ~dir =
  load_dir ~dir >>| function
  | Non_build _ -> Path.Build.Map.empty
  | Build { rules_here; _ } -> rules_here

let get_rule fn =
  match Path.as_in_build_dir fn with
  | None -> Memo.Build.return None
  | Some fn -> (
    let dir = Path.Build.parent_exn fn in
    load_dir ~dir:(Path.build dir) >>| function
    | Non_build _ -> assert false
    | Build { rules_here; _ } -> Path.Build.Map.find rules_here fn)

type rule_or_source =
  | Source of Digest.t
  | Rule of Path.Build.t * Rule.t

let get_rule_or_source t path =
  let dir = Path.parent_exn path in
  if Path.is_strict_descendant_of_build_dir dir then
    let* rules = load_dir_and_get_buildable_targets ~dir in
    let path = Path.as_in_build_dir_exn path in
    match Path.Build.Map.find rules path with
    | Some rule -> Memo.Build.return (Rule (path, rule))
    | None ->
      let* loc = Rule_fn.loc () in
      no_rule_found t ~loc path
  else
    let+ d = source_file_digest path in
    Source d

module Source_tree_map_reduce =
  Source_tree.Dir.Make_map_reduce (Memo.Build) (Monoid.Union (Path.Build.Set))

let all_targets t =
  let* root = Source_tree.root ()
  and* contexts = Memo.Lazy.force t.contexts in
  Memo.Build.parallel_map (Context_name.Map.values contexts) ~f:(fun ctx ->
      Source_tree_map_reduce.map_reduce root ~traverse:Sub_dirs.Status.Set.all
        ~f:(fun dir ->
          load_dir
            ~dir:
              (Path.build
                 (Path.Build.append_source ctx.Build_context.build_dir
                    (Source_tree.Dir.path dir)))
          >>| function
          | Non_build _ -> Path.Build.Set.empty
          | Build { rules_here; _ } ->
            Path.Build.Set.of_list (Path.Build.Map.keys rules_here)))
  >>| Path.Build.Set.union_all

let get_alias_definition alias =
  lookup_alias alias >>= function
  | None ->
    let open Pp.O in
    let+ loc = Rule_fn.loc () in
    User_error.raise ?loc
      [ Pp.text "No rule found for " ++ Alias.describe alias ]
  | Some x -> Memo.Build.return x

type rule_execution_result =
  { deps : Dep.Fact.t Dep.Map.t
  ; targets : Digest.t Path.Build.Map.t
  }

module type Rec = sig
  (** Build all the transitive dependencies of the alias and return the alias
      expansion. *)
  val build_alias : Alias.t -> Dep.Fact.Files.t Memo.Build.t

  val build_file : Path.t -> Digest.t Memo.Build.t

  val build_deps : Dep.Set.t -> Dep.Facts.t Memo.Build.t

  val eval_deps :
    'a Action_builder.eval_mode -> Dep.Set.t -> 'a Dep.Map.t Memo.Build.t

  val execute_rule : Rule.t -> rule_execution_result Memo.Build.t

  val execute_action :
    observing_facts:Dep.Facts.t -> Rule.Anonymous_action.t -> unit Memo.Build.t

  val execute_action_stdout :
       observing_facts:Dep.Facts.t
    -> Rule.Anonymous_action.t
    -> string Memo.Build.t

  module Pred : sig
    val eval : File_selector.t -> Path.Set.t Memo.Build.t

    val build : File_selector.t -> Dep.Fact.Files.t Memo.Build.t
  end
end

(* Separation between [Used_recursively] and [Exported] is necessary because at
   least one module in the recursive module group must be pure (i.e. only expose
   functions). *)
module rec Used_recursively : Rec = Exported

and Exported : sig
  include Rec

  val execute_rule : Rule.t -> rule_execution_result Memo.Build.t

  (* The below two definitions are useless, but if we remove them we get an
     "Undefined_recursive_module" exception. *)

  val build_file_memo : (Path.t, Digest.t) Memo.t [@@warning "-32"]

  val build_alias_memo : (Alias.t, Dep.Fact.Files.t) Memo.t [@@warning "-32"]

  val dep_on_alias_definition :
    Rules.Dir_rules.Alias_spec.item -> unit Action_builder.t
end = struct
  open Used_recursively

  (* [build_dep] turns a [Dep.t] which is a description of a dependency into a
     fact about the world. To do that, it needs to do some building. *)
  let build_dep : Dep.t -> Dep.Fact.t Memo.Build.t = function
    | Alias a ->
      let+ digests = build_alias a in
      (* Fact: alias [a] expand to the set of files with their digest
         [digests] *)
      Dep.Fact.alias a digests
    | File f ->
      let+ digest = build_file f in
      (* Fact: file [f] has digest [digest] *)
      Dep.Fact.file f digest
    | File_selector g ->
      let+ digests = Pred.build g in
      (* Fact: file selector [g] expands to the set of files with their digest
         [digests] *)
      Dep.Fact.file_selector g digests
    | Universe
    | Env _
    | Sandbox_config _ ->
      (* Facts about these dependencies are constructed in
         [Dep.Facts.digest]. *)
      Memo.Build.return Dep.Fact.nothing

  let build_deps deps =
    Dep.Map.parallel_map deps ~f:(fun dep () -> build_dep dep)

  let eval_deps :
      type a.
      a Action_builder.eval_mode -> Dep.Set.t -> a Dep.Map.t Memo.Build.t =
   fun mode deps ->
    match mode with
    | Lazy -> Memo.Build.return deps
    | Eager -> build_deps deps

  let select_sandbox_mode (config : Sandbox_config.t) ~loc
      ~sandboxing_preference =
    let evaluate_sandboxing_preference preference =
      let use_copy_on_windows mode =
        match Sandbox_mode.Set.mem config Sandbox_mode.copy with
        | true ->
          Some
            (if Sys.win32 then
              Sandbox_mode.copy
            else
              mode)
        | false ->
          User_error.raise ~loc
            [ Pp.textf
                "This rule requires sandboxing with %ss, but that won't work \
                 on Windows."
                (Sandbox_mode.to_string mode)
            ]
      in
      match Sandbox_mode.Set.mem config preference with
      | false -> None
      | true -> (
        match preference with
        | Some Symlink -> use_copy_on_windows Sandbox_mode.symlink
        | Some Hardlink -> use_copy_on_windows Sandbox_mode.hardlink
        | _ -> Some preference)
    in
    match
      List.find_map sandboxing_preference ~f:evaluate_sandboxing_preference
    with
    | Some choice -> choice
    | None ->
      (* This is not trivial to reach because the user rules are checked at
         parse time and [sandboxing_preference] always includes all possible
         modes. However, it can still be reached if multiple sandbox config
         specs are combined into an unsatisfiable one. *)
      User_error.raise ~loc
        [ Pp.text
            "This rule forbids all sandboxing modes (but it also requires \
             sandboxing)"
        ]

  let start_rule t _rule = t.rule_total <- t.rule_total + 1

  (* Same as [rename] except that if the source doesn't exist we delete the
     destination *)
  let rename_optional_file ~src ~dst =
    let src = Path.Build.to_string src in
    let dst = Path.Build.to_string dst in
    match Unix.rename src dst with
    | () -> ()
    | exception Unix.Unix_error ((ENOENT | ENOTDIR), _, _) -> (
      match Unix.unlink dst with
      | exception Unix.Unix_error (ENOENT, _, _) -> ()
      | () -> ())

  (* The current version of the rule digest scheme. We should increment it when
     making any changes to the scheme, to avoid collisions. *)
  let rule_digest_version = 7

  let compute_rule_digest (rule : Rule.t) ~deps ~action ~sandbox_mode
      ~execution_parameters =
    let { Action.Full.action; env; locks; can_go_in_shared_cache } = action in
    let trace =
      ( rule_digest_version (* Update when changing the rule digest scheme. *)
      , Dep.Facts.digest deps ~sandbox_mode ~env
      , Path.Build.Set.to_list_map rule.targets ~f:Path.Build.to_string
      , Option.map rule.context ~f:(fun c -> Context_name.to_string c.name)
      , Action.for_shell action
      , can_go_in_shared_cache
      , List.map locks ~f:Path.to_string
      , Execution_parameters.action_stdout_on_success execution_parameters
      , Execution_parameters.action_stderr_on_success execution_parameters )
    in
    Digest.generic trace

  let report_evaluated_rule build_system =
    Option.iter build_system.stats ~f:(fun stats ->
        let module Event = Chrome_trace.Event in
        let event =
          let args = [ ("value", `Int build_system.rule_total) ] in
          let ts = Event.Timestamp.now () in
          let common = Event.common_fields ~name:"evaluated_rules" ~ts () in
          Event.counter common args
        in
        Dune_stats.emit stats event)

  (** A type isomorphic to Result, but without the negative connotations
      assotiated with the word Error. *)
  module Cache_result = struct
    type ('hit, 'miss) t =
      | Hit of 'hit
      | Miss of 'miss
  end

  let shared_cache_key_string_for_log ~rule_digest ~head_target =
    sprintf "[%s] (%s)"
      (Digest.to_string rule_digest)
      (Path.Build.to_string head_target)

  module Shared_cache_miss_reason = struct
    type t =
      | Cache_disabled
      | Can't_go_in_shared_cache
      | Rerunning_for_reproducibility_check
      | Not_found_in_cache
      | Error of string
  end

  (* CR-someday amokhov: If the cloud cache is enabled, then before attempting
     to restore artifacts from the shared cache, we should send a download
     request for [rule_digest] to the cloud. *)
  let try_to_restore_from_shared_cache ~debug_shared_cache ~mode ~rule_digest
      ~head_target ~target_dir : (_, Shared_cache_miss_reason.t) Cache_result.t
      =
    let key () = shared_cache_key_string_for_log ~rule_digest ~head_target in
    match Dune_cache.Local.restore_artifacts ~mode ~rule_digest ~target_dir with
    | Restored res ->
      (* it's a small departure from the general "debug cache" semantics that
         we're also printing successes, but it can be useful to see successes
         too if the goal is to understand when and how the file in the build
         directory appeared *)
      if debug_shared_cache then
        Log.info [ Pp.textf "cache restore success %s" (key ()) ];
      Hit res
    | Not_found_in_cache -> Miss Not_found_in_cache
    | Error exn -> Miss (Error (Printexc.to_string exn))

  let execute_action_for_rule t ~rule_digest ~action ~deps ~loc
      ~(context : Build_context.t option) ~execution_parameters ~sandbox_mode
      ~dir ~targets =
    let open Fiber.O in
    let { Action.Full.action; env; locks; can_go_in_shared_cache = _ } =
      action
    in
    pending_targets := Path.Build.Set.union targets !pending_targets;
    let sandbox =
      Option.map sandbox_mode ~f:(fun mode ->
          let sandbox_suffix = rule_digest |> Digest.to_string in
          (Path.Build.relative sandbox_dir sandbox_suffix, mode))
    in
    let chdirs = Action.chdirs action in
    let sandboxed, action =
      match sandbox with
      | None -> (None, action)
      | Some (sandbox_dir, sandbox_mode) ->
        Path.rm_rf (Path.build sandbox_dir);
        let sandboxed path : Path.Build.t =
          Path.Build.append_local sandbox_dir (Path.Build.local path)
        in
        Path.Set.iter
          (Path.Set.union (Dep.Facts.dirs deps) chdirs)
          ~f:(fun path ->
            match Path.as_in_build_dir path with
            | None ->
              (* This [path] is not in the build directory, so we do not need to
                 create it. If it comes from [deps], it must exist already. If
                 it comes from [chdirs], we'll ensure that it exists in the call
                 to [Fs.mkdir_p_or_assert_existence] below. *)
              ()
            | Some path ->
              (* There is no point in using the memoized version [Fs.mkdir_p]
                 since these directories are not shared between actions. *)
              Path.mkdir_p (Path.build (sandboxed path)));
        Path.mkdir_p (Path.build (sandboxed dir));
        let deps =
          if
            Execution_parameters.should_expand_aliases_when_sandboxing
              execution_parameters
          then
            Dep.Facts.paths deps
          else
            Dep.Facts.paths_without_expanding_aliases deps
        in
        ( Some sandboxed
        , Action.sandbox action ~sandboxed ~mode:sandbox_mode ~deps )
    in
    let* () =
      Fiber.parallel_iter_set
        (module Path.Set)
        chdirs
        ~f:(fun p -> Memo.Build.run (Fs.mkdir_p_or_assert_existence ~loc p))
    in
    let build_deps deps = Memo.Build.run (build_deps deps) in
    let root =
      (match context with
      | None -> Path.Build.root
      | Some context -> context.build_dir)
      |> Option.value sandboxed ~default:Fun.id
      |> Path.build
    in
    let+ exec_result =
      with_locks t locks ~f:(fun () ->
          let copy_files_from_sandbox sandboxed =
            Path.Build.Set.iter targets ~f:(fun target ->
                rename_optional_file ~src:(sandboxed target) ~dst:target)
          in
          let+ exec_result =
            Action_exec.exec ~root ~context ~env ~targets ~rule_loc:loc
              ~build_deps ~execution_parameters action
          in
          Option.iter sandboxed ~f:copy_files_from_sandbox;
          exec_result)
    in
    Option.iter sandbox ~f:(fun (p, _mode) -> Path.rm_rf (Path.build p));
    (* All went well, these targets are no longer pending *)
    pending_targets := Path.Build.Set.diff !pending_targets targets;
    exec_result

  let try_to_store_to_shared_cache ~mode ~rule_digest ~action ~targets =
    let open Fiber.O in
    let hex = Digest.to_string rule_digest in
    let pp_error msg =
      let action = Action.for_shell action |> Action_to_sh.pp in
      Pp.concat
        [ Pp.textf "cache store error [%s]: %s after executing" hex msg
        ; Pp.space
        ; Pp.char '('
        ; action
        ; Pp.char ')'
        ]
    in
    let update_cached_digests ~targets_and_digests =
      List.iter targets_and_digests ~f:(fun (target, digest) ->
          Cached_digest.set (Path.build target) digest)
    in
    match
      Path.Build.Set.to_list_map targets ~f:Dune_cache.Local.Target.create
      |> Option.List.all
    with
    | None -> Fiber.return None
    | Some targets -> (
      let compute_digest ~executable path =
        Result.try_with (fun () ->
            Digest.file_with_executable_bit ~executable path)
        |> Fiber.return
      in
      Dune_cache.Local.store_artifacts ~mode ~rule_digest ~compute_digest
        targets
      >>| function
      | Stored targets_and_digests ->
        (* CR-someday amokhov: Here and in the case below we can inform the
           cloud daemon that a new cache entry can be uploaded to the cloud. *)
        Log.info [ Pp.textf "cache store success [%s]" hex ];
        update_cached_digests ~targets_and_digests;
        Some targets_and_digests
      | Already_present targets_and_digests ->
        Log.info [ Pp.textf "cache store skipped [%s]: already present" hex ];
        update_cached_digests ~targets_and_digests;
        Some targets_and_digests
      | Error exn ->
        Log.info [ pp_error (Printexc.to_string exn) ];
        None
      | Will_not_store_due_to_non_determinism sexp ->
        (* CR-someday amokhov: We should systematically log all warnings. *)
        Log.info [ pp_error (Sexp.to_string sexp) ];
        User_warning.emit [ pp_error (Sexp.to_string sexp) ];
        None)

  type rule_kind =
    | Normal_rule
    | Anonymous_action
    | Anonymous_action_attached_to_alias

  let report_workspace_local_cache_miss
      ~(cache_debug_flags : Cache_debug_flags.t) ~head_target reason =
    match cache_debug_flags.workspace_local_cache with
    | false -> ()
    | true ->
      let reason =
        match reason with
        | `No_previous_record -> "never seen this target before"
        | `Rule_changed (before, after) ->
          sprintf "rule or dependencies changed: %s -> %s"
            (Digest.to_string before) (Digest.to_string after)
        | `Targets_missing -> "target missing from build dir"
        | `Targets_changed -> "target changed in build dir"
        | `Always_rerun -> "not trying to use the cache"
        | `Dynamic_deps_changed -> "dynamic dependencies changed"
      in
      Log.info
        [ Pp.hbox
            (Pp.textf "Workspace-local cache miss: %s: %s\n"
               (Path.Build.to_string head_target)
               reason)
        ]

  let report_shared_cache_miss ~(cache_debug_flags : Cache_debug_flags.t)
      ~rule_digest ~head_target (reason : Shared_cache_miss_reason.t) =
    let should_print =
      match (cache_debug_flags.shared_cache, reason) with
      | true, _ -> true
      | false, Error _ ->
        (* always log errors because they are not expected as a part of normal
           operation and might indicate a problem *)
        true
      | false, _ -> false
    in
    match should_print with
    | false -> ()
    | true ->
      let reason =
        match reason with
        | Cache_disabled -> "cache disabled"
        | Can't_go_in_shared_cache -> "can't go in shared cache"
        | Error exn -> sprintf "error: %s" exn
        | Rerunning_for_reproducibility_check ->
          "rerunning for reproducibility check"
        | Not_found_in_cache -> "not found in cache"
      in
      Log.info
        [ Pp.hbox
            (Pp.textf "Shared cache miss %s: %s\n"
               (shared_cache_key_string_for_log ~rule_digest ~head_target)
               reason)
        ]

  let execute_rule_impl ~rule_kind rule =
    let t = t () in
    let { Rule.id = _; targets; dir; context; mode; action; info = _; loc } =
      rule
    in
    start_rule t rule;
    let head_target = Path.Build.Set.choose_exn targets in
    let* execution_parameters =
      match Dpath.Target_dir.of_target dir with
      | Regular (With_context (_, dir))
      | Anonymous_action (With_context (_, dir)) ->
        Source_tree.execution_parameters_of_dir dir
      | _ -> Execution_parameters.default
    in
    (* Note: we do not run the below in parallel with the above: if we fail to
       compute action execution parameters, we have no use for the action and
       might as well fail early, skipping unnecessary dependencies. The function
       [Source_tree.execution_parameters_of_dir] is memoized, and the result is
       not expected to change often, so we do not sacrifise too much performance
       here by executing it sequentially. *)
    let* action, deps = Action_builder.run action Eager in
    let wrap_fiber f =
      Memo.Build.of_reproducible_fiber
        (if Loc.is_none loc then
          f ()
        else
          Fiber.with_error_handler f ~on_error:(fun exn ->
              match exn.exn with
              | User_error.E (msg, annots)
                when not (User_error.has_location msg annots) ->
                let msg = { msg with loc = Some loc } in
                Exn_with_backtrace.reraise
                  { exn with exn = User_error.E (msg, annots) }
              | _ -> Exn_with_backtrace.reraise exn))
    in
    wrap_fiber (fun () ->
        let open Fiber.O in
        let build_deps deps = Memo.Build.run (build_deps deps) in
        report_evaluated_rule t;
        let* () = Memo.Build.run (Fs.mkdir_p dir) in
        let is_action_dynamic = Action.is_dynamic action.action in
        let sandbox_mode =
          match Action.is_useful_to_sandbox action.action with
          | Clearly_not ->
            let config = Dep.Map.sandbox_config deps in
            if Sandbox_config.mem config Sandbox_mode.none then
              Sandbox_mode.none
            else
              User_error.raise ~loc
                [ Pp.text
                    "Rule dependencies are configured to require sandboxing, \
                     but the rule has no actions that could potentially \
                     require sandboxing."
                ]
          | Maybe ->
            select_sandbox_mode ~loc
              (Dep.Map.sandbox_config deps)
              ~sandboxing_preference:t.sandboxing_preference
        in
        let always_rerun =
          let is_test =
            (* jeremiedimino: what about:

               {v (rule (alias runtest) (targets x) (action ...)) v}

               These will be treated as [Normal_rule], and the bellow match
               means that [--force] will have no effect on them. Is that what we
               want?

               The doc says:

               -f, --force Force actions associated to aliases to be re-executed
               even if their dependencies haven't changed.

               So it seems to me that such rules should be re-executed. TBC *)
            match rule_kind with
            | Normal_rule
            | Anonymous_action ->
              false
            | Anonymous_action_attached_to_alias -> true
          in
          let force_rerun = !Clflags.force && is_test in
          force_rerun || Dep.Map.has_universe deps
        in
        let rule_digest =
          compute_rule_digest rule ~deps ~action ~sandbox_mode
            ~execution_parameters
        in
        let can_go_in_shared_cache =
          action.can_go_in_shared_cache
          && not
               (always_rerun || is_action_dynamic
               || Action.is_useful_to_memoize action.action = Clearly_not)
        in
        (* We don't need to digest target names here, as these are already part
           of the rule digest. *)
        let digest_of_target_digests l = Digest.generic (List.map l ~f:snd) in
        (* Here we determine if we need to execute the action based on
           information stored in [Trace_db]. If we need to, then
           [targets_and_digests] will be [None], otherwise it will be [Some l]
           where [l] is the list of targets and their digests. *)
        let* (targets_and_digests :
               ((Path.Build.t * Digest.t) list, _) Cache_result.t) =
          if always_rerun then
            Fiber.return (Cache_result.Miss `Always_rerun)
          else
            (* [prev_trace] will be [None] if rule is run for the first time. *)
            let prev_trace = Trace_db.get (Path.build head_target) in
            let prev_trace_with_targets_and_digests =
              match prev_trace with
              | None -> Cache_result.Miss `No_previous_record
              | Some prev_trace -> (
                if prev_trace.rule_digest <> rule_digest then
                  Cache_result.Miss
                    (`Rule_changed (prev_trace.rule_digest, rule_digest))
                else
                  (* [targets_and_digests] will be [None] if not all targets
                     were built. *)
                  match compute_target_digests targets with
                  | None -> Cache_result.Miss `Targets_missing
                  | Some targets_and_digests ->
                    if
                      Digest.equal prev_trace.targets_digest
                        (digest_of_target_digests targets_and_digests)
                    then
                      Hit (prev_trace, targets_and_digests)
                    else
                      Cache_result.Miss `Targets_changed)
            in
            match prev_trace_with_targets_and_digests with
            | Cache_result.Miss reason ->
              Fiber.return (Cache_result.Miss reason)
            | Hit (prev_trace, targets_and_digests) ->
              (* CR-someday aalekseyev: If there's a change at one of the last
                 stages, we still re-run all the previous stages, which is a bit
                 of a waste. We could remember what stage needs re-running and
                 only re-run that (and later stages). *)
              let rec loop stages =
                match stages with
                | [] -> Fiber.return (Cache_result.Hit targets_and_digests)
                | (deps, old_digest) :: rest ->
                  let deps = Action_exec.Dynamic_dep.Set.to_dep_set deps in
                  let* deps = build_deps deps in
                  let new_digest =
                    Dep.Facts.digest deps ~sandbox_mode ~env:action.env
                  in
                  if old_digest = new_digest then
                    loop rest
                  else
                    Fiber.return (Cache_result.Miss `Dynamic_deps_changed)
              in
              loop prev_trace.dynamic_deps_stages
        in
        let* targets_and_digests =
          match targets_and_digests with
          | Hit x -> Fiber.return x
          | Miss miss_reason ->
            report_workspace_local_cache_miss
              ~cache_debug_flags:t.cache_debug_flags ~head_target miss_reason;
            (* Step I. Remove stale targets both from the digest table and from
               the build directory. *)
            Path.Build.Set.iter targets ~f:(fun target ->
                Cached_digest.remove (Path.build target);
                Path.Build.unlink_no_err target);
            (* Step II. Try to restore artifacts from the shared cache if the
               following conditions are met.

               1. The rule can be cached, i.e. [can_go_in_shared_cache] is
               [true].

               2. The shared cache is [Enabled].

               3. The rule is not selected for a reproducibility check. *)
            let targets_and_digests_from_cache :
                (_, Shared_cache_miss_reason.t) Cache_result.t =
              match (can_go_in_shared_cache, t.cache_config) with
              | false, _ ->
                Miss Shared_cache_miss_reason.Can't_go_in_shared_cache
              | _, Disabled -> Miss Shared_cache_miss_reason.Cache_disabled
              | true, Enabled { storage_mode = mode; reproducibility_check }
                -> (
                match
                  Dune_cache.Config.Reproducibility_check.sample
                    reproducibility_check
                with
                | true ->
                  (* CR-someday amokhov: Here we re-execute the rule, as in
                     Jenga. To make [check_probability] more meaningful, we
                     could first make sure that the shared cache actually does
                     contain an entry for [rule_digest]. *)
                  Cache_result.Miss
                    Shared_cache_miss_reason.Rerunning_for_reproducibility_check
                | false ->
                  try_to_restore_from_shared_cache
                    ~debug_shared_cache:t.cache_debug_flags.shared_cache ~mode
                    ~rule_digest ~head_target ~target_dir:rule.dir)
            in
            let* targets_and_digests, trace_db_entry =
              match targets_and_digests_from_cache with
              | Hit targets_and_digests ->
                Fiber.return
                  ( targets_and_digests
                  , ({ rule_digest
                     ; dynamic_deps_stages =
                         (* Rules with dynamic deps can't be stored to the
                            shared-cache (see the [is_action_dynamic] check
                            above), so we know this is not a dynamic action, so
                            returning an empty list is correct. The lack of
                            information to fill in [dynamic_deps_stages] here is
                            precisely the reason why we don't store dynamic
                            actions in the shared cache. *)
                         []
                     ; targets_digest =
                         digest_of_target_digests targets_and_digests
                     }
                      : Trace_db.Entry.t) )
              | Miss shared_cache_miss_reason ->
                report_shared_cache_miss ~cache_debug_flags:t.cache_debug_flags
                  ~rule_digest ~head_target shared_cache_miss_reason;
                (* Step III. Execute the build action. *)
                let* exec_result =
                  execute_action_for_rule t ~rule_digest ~action ~deps ~loc
                    ~context ~execution_parameters ~sandbox_mode ~dir ~targets
                in
                let* targets_and_digests =
                  (* Step IV. Store results to the shared cache and if that step
                     fails, post-process targets by removing write permissions
                     and computing their digets. *)
                  match t.cache_config with
                  | Enabled { storage_mode = mode; reproducibility_check = _ }
                    when can_go_in_shared_cache -> (
                    let+ targets_and_digests =
                      try_to_store_to_shared_cache ~mode ~rule_digest ~targets
                        ~action:action.action
                    in
                    match targets_and_digests with
                    | Some targets_and_digets -> targets_and_digets
                    | None ->
                      compute_target_digests_or_raise_error execution_parameters
                        ~loc targets)
                  | _ ->
                    Fiber.return
                      (compute_target_digests_or_raise_error
                         execution_parameters ~loc targets)
                in
                let dynamic_deps_stages =
                  List.map exec_result.dynamic_deps_stages
                    ~f:(fun (deps, fact_map) ->
                      ( deps
                      , Dep.Facts.digest fact_map ~sandbox_mode ~env:action.env
                      ))
                in
                let targets_digest =
                  digest_of_target_digests targets_and_digests
                in
                Fiber.return
                  ( targets_and_digests
                  , ({ rule_digest; dynamic_deps_stages; targets_digest }
                      : Trace_db.Entry.t) )
            in
            Trace_db.set (Path.build head_target) trace_db_entry;
            Fiber.return targets_and_digests
        in
        let* () =
          match (mode, !Clflags.promote) with
          | (Standard | Fallback | Ignore_source_files), _
          | Promote _, Some Never ->
            Fiber.return ()
          | Promote { lifetime; into; only }, (Some Automatically | None) ->
            Fiber.parallel_iter_set
              (module Path.Build.Set)
              targets
              ~f:(fun path ->
                let consider_for_promotion =
                  match only with
                  | None -> true
                  | Some pred ->
                    Predicate_lang.Glob.exec pred
                      (Path.reach (Path.build path) ~from:(Path.build dir))
                      ~standard:Predicate_lang.any
                in
                match consider_for_promotion with
                | false -> Fiber.return ()
                | true ->
                  let in_source_tree = Path.Build.drop_build_context_exn path in
                  let in_source_tree =
                    match into with
                    | None -> in_source_tree
                    | Some { loc; dir } ->
                      Path.Source.relative
                        (Path.Source.relative
                           (Path.Source.parent_exn in_source_tree)
                           dir ~error_loc:loc)
                        (Path.Source.basename in_source_tree)
                  in
                  let* () =
                    let dir = Path.Source.parent_exn in_source_tree in
                    Memo.Build.run (Source_tree.find_dir dir) >>| function
                    | Some _ -> ()
                    | None ->
                      let loc =
                        match into with
                        | Some into -> into.loc
                        | None ->
                          Code_error.raise
                            "promoting into directory that does not exist"
                            [ ( "in_source_tree"
                              , Path.Source.to_dyn in_source_tree )
                            ]
                      in
                      User_error.raise ~loc
                        [ Pp.textf "directory %S does not exist"
                            (Path.Source.to_string_maybe_quoted dir)
                        ]
                  in
                  let dst = in_source_tree in
                  let in_source_tree = Path.source in_source_tree in
                  let* is_up_to_date =
                    Memo.Build.run
                      (let open Memo.Build.O in
                      Fs_memo.path_exists in_source_tree >>= function
                      | false -> Memo.Build.return false
                      | true ->
                        let in_build_dir_digest =
                          Cached_digest.build_file path
                        in
                        let+ in_source_tree_digest =
                          Fs_memo.file_digest in_source_tree
                        in
                        Digest.equal in_build_dir_digest in_source_tree_digest)
                  in
                  if is_up_to_date then
                    Fiber.return ()
                  else (
                    if lifetime = Until_clean then
                      Promoted_to_delete.add in_source_tree;
                    let* () = Scheduler.ignore_for_watch in_source_tree in
                    (* The file in the build directory might be read-only if it
                       comes from the shared cache. However, we want the file in
                       the source tree to be writable by the user, so we
                       explicitly set the user writable bit. *)
                    let chmod n = n lor 0o200 in
                    Path.unlink_no_err (Path.source dst);
                    t.promote_source ~src:path ~dst ~chmod context
                  ))
        in
        t.rule_done <- t.rule_done + 1;
        let+ () =
          Handler.report_progress t.handler ~rule_done:t.rule_done
            ~rule_total:t.rule_total
        in
        targets_and_digests)
    (* jeremidimino: we need to include the dependencies discovered while
       running the action here. Otherwise, package dependencies are broken in
       the presence of dynamic actions *)
    >>|
    fun targets_and_digests ->
    { deps; targets = Path.Build.Map.of_list_exn targets_and_digests }

  module Anonymous_action = struct
    type t =
      { action : Rule.Anonymous_action.t
      ; deps : Dep.Set.t
      ; capture_stdout : bool
      ; digest : Digest.t
      }

    let equal a b = Digest.equal a.digest b.digest

    let hash t = Digest.hash t.digest

    let to_dyn t : Dyn.t =
      Record
        [ ("digest", Digest.to_dyn t.digest)
        ; ("loc", Dyn.Encoder.option Loc.to_dyn t.action.loc)
        ]
  end

  let execute_action_generic_stage2_impl
      { Anonymous_action.action = act; deps; capture_stdout; digest } =
    let target =
      let dir =
        Path.Build.append_local Dpath.Build.anonymous_actions_dir
          (Path.Build.local act.dir)
      in
      let d = Digest.to_string digest in
      let basename =
        match act.alias with
        | None -> d
        | Some a -> Alias.Name.to_string a ^ "-" ^ d
      in
      Path.Build.relative dir basename
    in
    let action =
      { act.action with
        action =
          (if capture_stdout then
            Action.with_stdout_to target act.action.action
          else
            Action.progn
              [ act.action.action; Action.with_stdout_to target Action.empty ])
      }
    in
    let rule =
      let { Rule.Anonymous_action.context; action = _; loc; dir = _; alias = _ }
          =
        act
      in
      Rule.make ~context
        ~info:
          (match loc with
          | Some loc -> From_dune_file loc
          | None -> Internal)
        ~targets:(Path.Build.Set.singleton target)
        (Action_builder.of_thunk
           { f =
               (fun mode ->
                 let+ deps = eval_deps mode deps in
                 (action, deps))
           })
    in
    let+ { deps = _; targets = _ } =
      execute_rule_impl rule
        ~rule_kind:
          (match act.alias with
          | None -> Anonymous_action
          | Some _ -> Anonymous_action_attached_to_alias)
    in
    target

  let execute_action_generic_stage2_memo =
    Memo.create "execute-action"
      ~input:(module Anonymous_action)
      execute_action_generic_stage2_impl

  let execute_action_generic ~observing_facts act ~capture_stdout =
    (* We memoize the execution of anonymous actions, both via the persisetent
       mechanism for not re-running build rules between invocations of [dune
       build] and via [Memo]. The former is done by producing a normal build
       rule on the fly for the anonymous action.

       Memoizing such actions via [Memo] doesn't feel super useful given that we
       expect a given anonymous action to be executed only once in a given
       build. And so persistent mechanism should be enough.

       However, if it does happen that two code paths try to execute the same
       anonymous action, then we need to be sure it is not executed twice. This
       is because the build rule we produce on the fly creates a file whose name
       only depend on the action. If the two execution could run concurrently,
       then they would both try to create the same file. So in this regard, we
       use [Memo] mostly for synchronisation purposes. *)
    (* Here we "forget" the facts about the world. We do that to make the input
       of the memoized function smaller. If we passed the whole [original_facts]
       as input, then we would end up memoizing one entry per set of facts. This
       could use a lot of memory. For instance, if we used [action_stdout] for
       the calls to [ocamldep], then Dune would remember the whole history of
       calls to [ocamldep] for each OCaml source file. *)
    let deps = Dep.Map.map observing_facts ~f:ignore in
    (* Shadow [observing_facts] to make sure we don't use it again. *)
    let observing_facts = () in
    ignore observing_facts;
    let digest =
      let { Rule.Anonymous_action.context
          ; action = { action; env; locks; can_go_in_shared_cache }
          ; loc
          ; dir
          ; alias
          } =
        act
      in
      let env =
        (* Here we restrict the environment to only the variables we depend on,
           so that we don't re-execute all actions when some irrelevant
           environment variable changes.

           Ideally, we would pass this restricted environment to the external
           command, however that might be tedious to do in practice. See this
           ticket for a longer discussion about the management of the
           environment: https://github.com/ocaml/dune/issues/4382 *)
        Dep.Set.fold deps ~init:Env.Map.empty ~f:(fun dep acc ->
            match dep with
            | Env var -> Env.Map.set acc var (Env.get env var)
            | _ -> acc)
        |> Env.Map.to_list
      in
      Digest.generic
        ( Option.map context ~f:(fun c ->
              (* Only looking at the context name is fishy, but it is in line
                 with what we do for build rules. *)
              Context_name.to_string c.name)
        , env
        , Dep.Set.digest deps
        , Action.for_shell action
        , List.map locks ~f:Path.to_string
        , loc
        , dir
        , alias
        , capture_stdout
        , can_go_in_shared_cache )
    in
    (* It might seem superfluous to memoize the execution here, given that a
       given anonymous action will typically only appear once during a given
       build. However, it is possible that two code paths try to execute the
       exact same anonymous action, and so would end up trying to create the
       same file. Using [Memo.create] serves as a synchronisation point to share
       the execution and avoid such a race condition. *)
    Memo.exec execute_action_generic_stage2_memo
      { action = act; deps; capture_stdout; digest }

  let execute_action ~observing_facts act =
    let+ _target =
      execute_action_generic ~observing_facts act ~capture_stdout:false
    in
    ()

  let execute_action_stdout ~observing_facts act =
    let+ target =
      execute_action_generic ~observing_facts act ~capture_stdout:true
    in
    Io.read_file (Path.build target)

  (* A rule can have multiple targets but calls to [execute_rule] are memoized,
     so the rule will be executed only once.

     [build_file_impl] returns both the set of dependencies of the file as well
     as its digest. *)
  let build_file_impl path =
    let t = t () in
    get_rule_or_source t path >>= function
    | Source digest -> Memo.Build.return digest
    | Rule (path, rule) ->
      let+ { deps = _; targets } =
        Memo.push_stack_frame
          (fun () -> execute_rule rule)
          ~human_readable_description:(fun () ->
            Pp.text (Path.to_string_maybe_quoted (Path.build path)))
      in
      Path.Build.Map.find_exn targets path

  let dep_on_anonymous_action (x : Rule.Anonymous_action.t Action_builder.t) :
      _ Action_builder.t =
    Action_builder.of_thunk
      { f =
          (fun (type m) (mode : m Action_builder.eval_mode) ->
            match mode with
            | Lazy -> Memo.Build.return ((), Dep.Map.empty)
            | Eager ->
              let* action, facts = Action_builder.run x Eager in
              let+ () = execute_action action ~observing_facts:facts in
              ((), Dep.Map.empty))
      }

  let dep_on_alias_definition (definition : Rules.Dir_rules.Alias_spec.item) =
    match definition with
    | Deps x -> x
    | Action x -> dep_on_anonymous_action x

  let build_alias_impl alias =
    let+ l =
      get_alias_definition alias
      >>= Memo.Build.parallel_map ~f:(fun (loc, definition) ->
              Memo.push_stack_frame
                (fun () ->
                  Action_builder.run (dep_on_alias_definition definition) Eager
                  >>| snd)
                ~human_readable_description:(fun () ->
                  Alias.describe alias ~loc))
    in
    Dep.Facts.group_paths_as_fact_files l

  module Pred = struct
    let build_impl g =
      let* paths = Pred.eval g in
      let+ files =
        Memo.Build.parallel_map (Path.Set.to_list paths) ~f:(fun p ->
            let+ d = build_file p in
            (p, d))
      in
      Dep.Fact.Files.make (Path.Map.of_list_exn files)

    let eval_impl g =
      let dir = File_selector.dir g in
      load_dir ~dir >>| function
      | Non_build targets -> Path.Set.filter targets ~f:(File_selector.test g)
      | Build { rules_here; _ } ->
        let only_generated_files = File_selector.only_generated_files g in
        Path.Build.Map.foldi ~init:[] rules_here
          ~f:(fun s { Rule.info; _ } acc ->
            match info with
            | Rule.Info.Source_file_copy _ when only_generated_files -> acc
            | _ ->
              let s = Path.build s in
              if File_selector.test g s then
                s :: acc
              else
                acc)
        |> Path.Set.of_list

    let eval_memo =
      Memo.create "eval-pred"
        ~input:(module File_selector)
        ~cutoff:Path.Set.equal eval_impl

    let eval = Memo.exec eval_memo

    let build =
      Memo.exec
        (Memo.create "build-pred"
           ~input:(module File_selector)
           ~cutoff:Dep.Fact.Files.equal build_impl)
  end

  let build_file_memo =
    Memo.create "build-file"
      ~input:(module Path)
      ~cutoff:Digest.equal build_file_impl

  let build_file = Memo.exec build_file_memo

  let build_alias_memo =
    Memo.create "build-alias"
      ~input:(module Alias)
      ~cutoff:Dep.Fact.Files.equal build_alias_impl

  let build_alias = Memo.exec build_alias_memo

  let execute_rule_memo =
    Memo.create "execute-rule"
      ~input:(module Rule)
      (execute_rule_impl ~rule_kind:Normal_rule)

  let execute_rule = Memo.exec execute_rule_memo

  let () =
    Fdecl.set Rule_fn.loc_decl (fun () ->
        let+ stack = Memo.get_call_stack () in
        List.find_map stack ~f:(fun frame ->
            match
              Memo.Stack_frame.as_instance_of frame ~of_:execute_rule_memo
            with
            | Some r -> Some (Rule.loc r)
            | None ->
              Option.bind
                (Memo.Stack_frame.as_instance_of frame
                   ~of_:execute_action_generic_stage2_memo) ~f:(fun x ->
                  x.action.Rule.Anonymous_action.loc)))
end

open Exported

type alias_definition = Rules.Dir_rules.Alias_spec.item

let dep_on_alias_definition = dep_on_alias_definition

let eval_pred = Pred.eval

let build_pred = Pred.build

let package_deps ~packages_of (pkg : Package.t) files =
  let rules_seen = ref Rule.Set.empty in
  let rec loop fn =
    match Path.as_in_build_dir fn with
    | None ->
      (* if this file isn't in the build dir, it doesn't belong to any package
         and it doesn't have dependencies that do *)
      Memo.Build.return Package.Id.Set.empty
    | Some fn ->
      let* pkgs = packages_of fn in
      if Package.Id.Set.is_empty pkgs || Package.Id.Set.mem pkgs pkg.id then
        loop_deps fn
      else
        Memo.Build.return pkgs
  and loop_deps fn =
    get_rule (Path.build fn) >>= function
    | None -> Memo.Build.return Package.Id.Set.empty
    | Some rule ->
      if Rule.Set.mem !rules_seen rule then
        Memo.Build.return Package.Id.Set.empty
      else (
        rules_seen := Rule.Set.add !rules_seen rule;
        let* res = execute_rule rule in
        loop_files (Dep.Facts.paths res.deps |> Path.Map.keys)
      )
  and loop_files files =
    let+ sets = Memo.Build.parallel_map files ~f:loop in
    List.fold_left sets ~init:Package.Id.Set.empty ~f:Package.Id.Set.union
  in
  loop_files (Path.Set.to_list files)

let caused_by_cancellation (exn : Exn_with_backtrace.t) =
  match exn.exn with
  | Scheduler.Run.Build_cancelled -> true
  | Memo.Error.E err -> (
    match Memo.Error.get err with
    | Scheduler.Run.Build_cancelled -> true
    | _ -> false)
  | _ -> false

let report_early_exn exn =
  let t = t () in
  match caused_by_cancellation exn with
  | true -> Fiber.return ()
  | false ->
    let error = { Error.exn; id = Error.Id.gen () } in
    t.errors <- error :: t.errors;
    (match !Clflags.report_errors_config with
    | Early
    | Twice ->
      Dune_util.Report_error.report exn
    | Deterministic -> ());
    t.handler.error [ Add error ]

let handle_final_exns exns =
  match !Clflags.report_errors_config with
  | Early -> ()
  | Twice
  | Deterministic ->
    let report exn =
      if not (caused_by_cancellation exn) then Dune_util.Report_error.report exn
    in
    List.iter exns ~f:report

let run f =
  let open Fiber.O in
  Hooks.End_of_build.once Promotion.finalize;
  let t = t () in
  let old_errors = t.errors in
  t.errors <- [];
  t.rule_done <- 0;
  t.rule_total <- 0;
  let* () =
    match old_errors with
    | [] -> Fiber.return ()
    | _ :: _ ->
      t.handler.error (List.map old_errors ~f:(fun x -> Handler.Remove x))
  in
  let f () =
    let* () = Handler.report_build_event t.handler Start in
    let build_end_status_reported = ref false in
    let* res =
      Fiber.collect_errors (fun () ->
          Memo.Build.run_with_error_handler (f ())
            ~handle_error_no_raise:(fun exn ->
              let* () = report_early_exn exn in
              if !build_end_status_reported then
                Fiber.return ()
              else (
                build_end_status_reported := true;
                Handler.report_build_event t.handler
                  (if caused_by_cancellation exn then
                    Interrupt
                  else
                    Fail)
              )))
    in
    match res with
    | Ok res ->
      let+ () =
        if !build_end_status_reported then
          Fiber.return ()
        else
          Handler.report_build_event t.handler Finish
      in
      Ok res
    | Error exns ->
      handle_final_exns exns;
      Fiber.return (Error `Already_reported)
  in
  Fiber.Mutex.with_lock t.build_mutex f

let run_exn f =
  let open Fiber.O in
  let+ res = run f in
  match res with
  | Ok res -> res
  | Error `Already_reported -> raise Dune_util.Report_error.Already_reported

let build_file = build_file

let read_file p ~f =
  let+ _digest = build_file p in
  f p

let build_deps = build_deps

let file_exists = file_exists

let alias_exists = Load_rules.alias_exists

let is_target file =
  let+ targets = targets_of ~dir:(Path.parent_exn file) in
  Path.Set.mem targets file

let execute_action = execute_action

let execute_action_stdout = execute_action_stdout

let load_dir_and_produce_its_rules ~dir =
  load_dir ~dir >>= function
  | Non_build _ -> Memo.Build.return ()
  | Build loaded -> Rules.produce loaded.rules_produced

let load_dir ~dir = load_dir_and_produce_its_rules ~dir

let init ~stats ~contexts ~promote_source ~cache_config ~cache_debug_flags
    ~sandboxing_preference ~rule_generator ~handler ~implicit_default_alias =
  let contexts =
    Memo.lazy_ (fun () ->
        let+ contexts = Memo.Lazy.force contexts in
        Context_name.Map.of_list_map_exn contexts ~f:(fun c ->
            (c.Build_context.name, c)))
  in
  let () =
    match (cache_config : Dune_cache.Config.t) with
    | Disabled -> ()
    | Enabled _ -> Dune_cache_storage.Layout.create_cache_directories ()
  in
  set
    { contexts
    ; rule_generator
    ; sandboxing_preference = sandboxing_preference @ Sandbox_mode.all
    ; rule_done = 0
    ; rule_total = 0
    ; errors = []
    ; handler = Option.value handler ~default:Handler.do_nothing
    ; (* This mutable table is safe: it merely maps paths to lazily created
         mutexes. *)
      locks = Table.create (module Path) 32
    ; promote_source
    ; build_mutex = Fiber.Mutex.create ()
    ; stats
    ; cache_config
    ; cache_debug_flags
    ; implicit_default_alias
    }

module Progress = struct
  type t =
    { number_of_rules_discovered : int
    ; number_of_rules_executed : int
    }
end

let get_current_progress () =
  let t = t () in
  { Progress.number_of_rules_executed = t.rule_done
  ; number_of_rules_discovered = t.rule_total
  }

let targets_of = targets_of

let all_targets () = all_targets (t ())

let last_event () = !Handler.last_event
