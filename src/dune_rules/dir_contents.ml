open! Dune_engine
open! Stdune
open Import
module Menhir_rules = Menhir
open Dune_file
open! No_io
open Memo.Build.O

let loc_of_dune_file st_dir =
  Loc.in_file
    (Path.source
       (match Source_tree.Dir.dune_file st_dir with
       | Some d -> Source_tree.Dune_file.path d
       | None -> Path.Source.relative (Source_tree.Dir.path st_dir) "_unknown_"))

type t =
  { kind : kind
  ; dir : Path.Build.t
  ; text_files : String.Set.t
  ; foreign_sources : Foreign_sources.t Memo.Lazy.t
  ; mlds : (Dune_file.Documentation.t * Path.Build.t list) list Memo.Lazy.t
  ; coq : Coq_sources.t Memo.Lazy.t
  ; ml : Ml_sources.t Memo.Lazy.t
  }

and kind =
  | Standalone
  | Group_root of t list
  | Group_part

let empty kind ~dir =
  { kind
  ; dir
  ; text_files = String.Set.empty
  ; ml = Memo.Lazy.of_val Ml_sources.empty
  ; mlds = Memo.Lazy.of_val []
  ; foreign_sources = Memo.Lazy.of_val Foreign_sources.empty
  ; coq = Memo.Lazy.of_val Coq_sources.empty
  }

type gen_rules_result =
  | Standalone_or_root of t * t list
  | Group_part of Path.Build.t

let dir t = t.dir

let coq t = Memo.Lazy.force t.coq

let ocaml t = Memo.Lazy.force t.ml

let artifacts t = Memo.Lazy.force t.ml >>= Ml_sources.artifacts

let dirs t =
  match t.kind with
  | Standalone -> [ t ]
  | Group_root subs -> t :: subs
  | Group_part ->
    Code_error.raise "Dir_contents.dirs called on a group part"
      [ ("dir", Path.Build.to_dyn t.dir) ]

let text_files t = t.text_files

let foreign_sources t = Memo.Lazy.force t.foreign_sources

let mlds t (doc : Documentation.t) =
  let+ map = Memo.Lazy.force t.mlds in
  match
    List.find_map map ~f:(fun (doc', x) ->
        Option.some_if (Loc.equal doc.loc doc'.loc) x)
  with
  | Some x -> x
  | None ->
    Code_error.raise "Dir_contents.mlds"
      [ ("doc", Loc.to_dyn_hum doc.loc)
      ; ( "available"
        , Dyn.Encoder.(list Loc.to_dyn_hum)
            (List.map map ~f:(fun (d, _) -> d.Documentation.loc)) )
      ]

let build_mlds_map (d : _ Dir_with_dune.t) ~files =
  let dir = d.ctx_dir in
  let mlds =
    Memo.lazy_ (fun () ->
        String.Set.fold files ~init:String.Map.empty ~f:(fun fn acc ->
            match String.lsplit2 fn ~on:'.' with
            | Some (s, "mld") -> String.Map.set acc s fn
            | _ -> acc)
        |> Memo.Build.return)
  in
  Memo.Build.parallel_map d.data ~f:(function
    | Documentation doc ->
      let+ mlds =
        let+ mlds = Memo.Lazy.force mlds in
        Ordered_set_lang.Unordered_string.eval doc.mld_files
          ~key:(fun x -> x)
          ~parse:(fun ~loc s ->
            match String.Map.find mlds s with
            | Some s -> s
            | None ->
              User_error.raise ~loc
                [ Pp.textf "%s.mld doesn't exist in %s" s
                    (Path.to_string_maybe_quoted
                       (Path.drop_optional_build_context (Path.build dir)))
                ])
          ~standard:mlds
      in
      Some (doc, List.map (String.Map.values mlds) ~f:(Path.Build.relative dir))
    | _ -> Memo.Build.return None)
  >>| List.filter_map ~f:Fun.id

module rec Load : sig
  val get : Super_context.t -> dir:Path.Build.t -> t Memo.Build.t

  val gen_rules :
    Super_context.t -> dir:Path.Build.t -> gen_rules_result Memo.Build.t

  val add_sources_to_expander : Super_context.t -> Expander.t -> Expander.t
end = struct
  let add_sources_to_expander sctx expander =
    let f ~dir = Load.get sctx ~dir >>= artifacts in
    Expander.set_lookup_ml_sources expander ~f

  (* As a side-effect, setup user rules and copy_files rules. *)
  let load_text_files sctx st_dir
      { Dir_with_dune.ctx_dir = dir
      ; src_dir
      ; scope = _
      ; data = stanzas
      ; dune_version = _
      } =
    (* Interpret a few stanzas in order to determine the list of files generated
       by the user. *)
    let* expander =
      Super_context.expander sctx ~dir >>| add_sources_to_expander sctx
    in
    let+ generated_files =
      Memo.Build.parallel_map stanzas ~f:(fun stanza ->
          match (stanza : Stanza.t) with
          (* XXX What about mli files? *)
          | Coq_stanza.Coqpp.T { modules; _ } ->
            Memo.Build.return (List.map modules ~f:(fun m -> m ^ ".ml"))
          | Coq_stanza.Extraction.T s ->
            Memo.Build.return (Coq_stanza.Extraction.ml_target_fnames s)
          | Menhir.T menhir -> Memo.Build.return (Menhir_rules.targets menhir)
          | Rule rule ->
            Simple_rules.user_rule sctx rule ~dir ~expander
            >>| Path.Build.Set.to_list_map ~f:Path.Build.basename
          | Copy_files def ->
            let+ ps =
              Simple_rules.copy_files sctx def ~src_dir ~dir ~expander
            in
            Path.Set.to_list_map ps ~f:Path.basename
          | Generate_sites_module def ->
            let+ res = Generate_sites_module_rules.setup_rules sctx ~dir def in
            [ res ]
          | Library { buildable; _ } | Executables { buildable; _ } ->
            let select_deps_files =
              (* Manually add files generated by the (select ...) dependencies *)
              List.filter_map buildable.libraries ~f:(fun dep ->
                match (dep : Lib_dep.t) with
                | Re_export _
                | Direct _ ->
                  None
                | Select s -> Some s.result_fn)
            in
            let ctypes_files =
              (* Also manually add files generated by ctypes rules. *)
              match buildable.ctypes with
              | None -> []
              | Some ctypes ->
                Ctypes_rules.generated_ml_and_c_files ctypes
            in
            Memo.Build.return (select_deps_files @ ctypes_files)
          | _ -> Memo.Build.return [])
      >>| fun l -> String.Set.of_list (List.concat l)
    in
    String.Set.union generated_files (Source_tree.Dir.files st_dir)

  type result0_here =
    { t : t
    ; (* [rules] includes rules for subdirectories too *)
      rules : Rules.t option
    ; (* The [kind] of the nodes must be Group_part *)
      subdirs : t Path.Build.Map.t
    }

  type result0 =
    | See_above of Path.Build.t
    | Here of result0_here

  module Key = struct
    module Super_context = Super_context.As_memo_key

    type t = Super_context.t * Path.Build.t

    let to_dyn (sctx, path) =
      Dyn.Tuple [ Super_context.to_dyn sctx; Path.Build.to_dyn path ]

    let equal = Tuple.T2.equal Super_context.equal Path.Build.equal

    let hash = Tuple.T2.hash Super_context.hash Path.Build.hash
  end

  let lookup_vlib sctx ~dir =
    let* t = Load.get sctx ~dir in
    Memo.Lazy.force t.ml

  let collect_group sctx ~st_dir ~dir =
    let dir_status_db = Super_context.dir_status_db sctx in
    let rec walk st_dir ~dir ~local =
      let* status = Dir_status.DB.get dir_status_db ~dir in
      match status with
      | Is_component_of_a_group_but_not_the_root { stanzas = d; group_root = _ }
        ->
        let+ a, b =
          Memo.Build.fork_and_join
            (fun () ->
              let+ files =
                match d with
                | None -> Memo.Build.return (Source_tree.Dir.files st_dir)
                | Some d -> load_text_files sctx st_dir d
              in
              Appendable_list.singleton (dir, List.rev local, files))
            (fun () -> walk_children st_dir ~dir ~local)
        in
        Appendable_list.( @ ) a b
      | Generated
      | Source_only _
      | Standalone _
      | Group_root _ ->
        Memo.Build.return Appendable_list.empty
    and walk_children st_dir ~dir ~local =
      let+ l =
        Memo.Build.parallel_map
          (Source_tree.Dir.sub_dirs st_dir |> String.Map.to_list)
          ~f:(fun (basename, st_dir) ->
            let* st_dir = Source_tree.Dir.sub_dir_as_t st_dir in
            let dir = Path.Build.relative dir basename in
            let local = basename :: local in
            walk st_dir ~dir ~local)
      in
      Appendable_list.concat l
    in
    let+ l = walk_children st_dir ~dir ~local:[] in
    Appendable_list.to_list l

  let get0_impl (sctx, dir) : result0 Memo.Build.t =
    let dir_status_db = Super_context.dir_status_db sctx in
    let ctx = Super_context.context sctx in
    let lib_config = (Super_context.context sctx).lib_config in
    let* status = Dir_status.DB.get dir_status_db ~dir in
    match status with
    | Is_component_of_a_group_but_not_the_root { group_root; stanzas = _ } ->
      Memo.Build.return (See_above group_root)
    | Generated
    | Source_only _ ->
      Memo.Build.return
        (Here
           { t = empty Standalone ~dir
           ; rules = None
           ; subdirs = Path.Build.Map.empty
           })
    | Standalone (st_dir, d) ->
      let include_subdirs = (Loc.none, Include_subdirs.No) in
      let+ files, rules =
        Rules.collect_opt (fun () -> load_text_files sctx st_dir d)
      in
      let dirs = [ (dir, [], files) ] in
      let ml =
        Memo.lazy_ (fun () ->
            let lookup_vlib = lookup_vlib sctx in
            let loc = loc_of_dune_file st_dir in
            Ml_sources.make d ~lib_config ~loc ~include_subdirs ~lookup_vlib
              ~dirs)
      in
      Here
        { t =
            { kind = Standalone
            ; dir
            ; text_files = files
            ; ml
            ; mlds = Memo.lazy_ (fun () -> build_mlds_map d ~files)
            ; foreign_sources =
                Memo.lazy_ (fun () ->
                    Foreign_sources.make d ~lib_config:ctx.lib_config
                      ~include_subdirs ~dirs
                    |> Memo.Build.return)
            ; coq =
                Memo.lazy_ (fun () ->
                    Coq_sources.of_dir d ~include_subdirs ~dirs
                    |> Memo.Build.return)
            }
        ; rules
        ; subdirs = Path.Build.Map.empty
        }
    | Group_root (st_dir, qualif_mode, d) ->
      let loc = loc_of_dune_file st_dir in
      let include_subdirs =
        let loc, qualif_mode = qualif_mode in
        (loc, Dune_file.Include_subdirs.Include qualif_mode)
      in
      let+ (files, (subdirs : (Path.Build.t * _ * _) list)), rules =
        Rules.collect_opt (fun () ->
            Memo.Build.fork_and_join
              (fun () -> load_text_files sctx st_dir d)
              (fun () -> collect_group sctx ~st_dir ~dir))
      in
      let dirs = (dir, [], files) :: subdirs in
      let ml =
        Memo.lazy_ (fun () ->
            let lookup_vlib = lookup_vlib sctx in
            Ml_sources.make d ~lib_config ~loc ~lookup_vlib ~include_subdirs
              ~dirs)
      in
      let foreign_sources =
        Memo.lazy_ (fun () ->
            Foreign_sources.make d ~include_subdirs ~lib_config:ctx.lib_config
              ~dirs
            |> Memo.Build.return)
      in
      let coq =
        Memo.lazy_ (fun () ->
            Coq_sources.of_dir d ~dirs ~include_subdirs |> Memo.Build.return)
      in
      let subdirs =
        List.map subdirs ~f:(fun (dir, _local, files) ->
            { kind = Group_part
            ; dir
            ; text_files = files
            ; ml
            ; foreign_sources
            ; mlds = Memo.lazy_ (fun () -> build_mlds_map d ~files)
            ; coq
            })
      in
      let t =
        { kind = Group_root subdirs
        ; dir
        ; text_files = files
        ; ml
        ; foreign_sources
        ; mlds = Memo.lazy_ (fun () -> build_mlds_map d ~files)
        ; coq
        }
      in
      Here
        { t
        ; rules
        ; subdirs =
            Path.Build.Map.of_list_map_exn subdirs ~f:(fun x -> (x.dir, x))
        }

  let memo0 = Memo.create "dir-contents-get0" ~input:(module Key) get0_impl

  let get sctx ~dir =
    Memo.exec memo0 (sctx, dir) >>= function
    | Here { t; rules = _; subdirs = _ } -> Memo.Build.return t
    | See_above group_root -> (
      Memo.exec memo0 (sctx, group_root) >>| function
      | See_above _ -> assert false
      | Here { t; rules = _; subdirs = _ } -> t)

  let () =
    let f sctx ~dir ~name =
      let* t = get sctx ~dir in
      let+ ml_sources = ocaml t in
      Ml_sources.modules ml_sources ~for_:(Library name)
    in
    Fdecl.set Super_context.modules_of_lib f

  let gen_rules sctx ~dir =
    Memo.exec memo0 (sctx, dir) >>= function
    | See_above group_root -> Memo.Build.return (Group_part group_root)
    | Here { t; rules; subdirs } ->
      let+ () = Rules.produce_opt rules in
      Standalone_or_root (t, Path.Build.Map.values subdirs)
end

include Load
