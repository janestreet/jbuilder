open Stdune
open Dune_cache_storage.Layout
open Fiber.O
module Store_result = Dune_cache_storage.Store_result
module Restore_result = Dune_cache_storage.Restore_result

module Store_artifacts_result = struct
  type t =
    | Stored of (Path.Build.t * Digest.t) list
    | Already_present of (Path.Build.t * Digest.t) list
    | Error of exn
    | Will_not_store_due_to_non_determinism of Sexp.t

  let of_store_result ~artifacts t =
    match (t : Store_result.t) with
    | Stored -> Stored artifacts
    | Already_present -> Already_present artifacts
    | Error exn -> Error exn
    | Will_not_store_due_to_non_determinism details ->
      Will_not_store_due_to_non_determinism details

  let bind t ~f =
    match t with
    | Stored data -> f data
    | Already_present data -> f data
    | (Error _ | Will_not_store_due_to_non_determinism _) as res -> res
end

module Target = struct
  type t =
    { path : Path.Build.t
    ; executable : bool
    }

  let create path =
    match Path.Build.lstat path with
    | { Unix.st_kind = Unix.S_REG; st_perm; _ } ->
      Path.Build.chmod path
        ~mode:(Path.Permissions.remove ~mode:Path.Permissions.write st_perm);
      let executable = st_perm land 0o100 <> 0 in
      Some { path; executable }
    | (exception _)
    | _ ->
      None
end

(* This function is like [Unix.link] but handles the "Too many links" error by
   creating a copy of the [src] in a temporary directory, then atomically
   replacing the [src] with the copy, and finally creating the requested [dst]
   by calling [Unix.link] again.

   We hit the "Too many links" error because we store a lot of empty files in
   the cache, which all get deduplicated into the same cache entry. This
   function essentially deletes the "overlinked" entry from the cache, creating
   a fresh copy with the 0 link count. This leads to some duplication but it's
   negligible: we might store the empty file several times across all workspaces
   instead of just storing it once. *)
let link_even_if_there_are_too_many_links_already ~src ~dst =
  try Path.link src dst with
  | Unix.Unix_error (Unix.EMLINK, _, _) ->
    Temp.with_temp_file ~dir:temp_dir ~prefix:"dune" ~suffix:"copy" ~f:(function
      | Error e -> raise e
      | Ok temp_file ->
        Io.copy_file ~src ~dst:temp_file ();
        Path.rename temp_file dst;
        Path.link src dst)

module Artifacts = struct
  include Dune_cache_storage.Artifacts

  let store_metadata ~mode ~metadata ~rule_digest
      (artifacts : (Path.Build.t * Digest.t) list) =
    let entries =
      List.map artifacts ~f:(fun (target, file_digest) ->
          let entry : Metadata_entry.t =
            { file_name = Path.Build.basename target; file_digest }
          in
          entry)
    in
    Metadata_file.store ~mode { metadata; entries } ~rule_digest

  (* Step I of [store_skipping_metadata].

     If any of the targets couldn't be stored in the temporary directory, then
     the result is [Error] with the corresponding exception. Otherwise, the
     result is [Ok ()]. *)
  let store_targets_to ~temp_dir ~targets ~mode : unit Or_exn.t =
    Result.List.fold_left targets ~init:() ~f:(fun () { Target.path; _ } ->
        let path_in_build_dir = Path.build path in
        let path_in_temp_dir =
          Path.relative temp_dir (Path.basename path_in_build_dir)
        in
        Result.try_with (fun () ->
            Dune_cache_storage.Util.link_or_copy ~mode ~src:path_in_build_dir
              ~dst:path_in_temp_dir))

  (* Step II of [store_skipping_metadata].

     Computing digests can be slow, so we do that in parallel. *)
  let compute_digests_in ~temp_dir ~targets ~compute_digest :
      (Path.Build.t * Digest.t) list Or_exn.t Fiber.t =
    let open Fiber.O in
    Fiber.parallel_map targets ~f:(fun { Target.path; executable } ->
        let file = Path.relative temp_dir (Path.Build.basename path) in
        compute_digest ~executable file
        >>| Or_exn.map ~f:(fun digest -> (path, digest)))
    >>| Result.List.all

  (* Step III of [store_skipping_metadata]. *)
  let store_to_cache_from ~temp_dir ~mode
      (artifacts : (Path.Build.t * Digest.t) list) =
    List.fold_left artifacts ~init:Store_result.empty
      ~f:(fun results (target, digest) ->
        let file_name = Path.Build.basename target in
        let path_in_temp_dir = Path.relative temp_dir file_name in
        let path_in_cache = file_path ~file_digest:digest in
        let store_using_hardlinks () =
          match
            Dune_cache_storage.Util.Optimistically.link ~src:path_in_temp_dir
              ~dst:path_in_cache
          with
          | exception Unix.Unix_error (Unix.EEXIST, _, _) -> (
            (* We end up here if the cache already contains an entry for this
               artifact. We deduplicate by keeping only one copy, in the
               cache. *)
            let path_in_build_dir = Path.build target in
            match
              Path.unlink_no_err path_in_temp_dir;
              (* At first, we deduplicate the temporary file. Doing this
                 intermediate step allows us to keep the original target in case
                 the below link step fails. This might happen if the trimmer has
                 just deleted [path_in_cache]. In this rare case, this function
                 fails with an [Error], and so we might end up with some
                 duplicates in the workspace. *)
              link_even_if_there_are_too_many_links_already ~src:path_in_cache
                ~dst:path_in_temp_dir;
              (* Now we can simply rename the temporary file into the target,
                 knowing that the original target remains in place if the
                 renaming fails.

                 One curious case to think about is if the file in the cache
                 happens to have the same inode as the file in the workspace. In
                 that case this deduplication should be a no-op, but the
                 [rename] operation has a quirk where [path_in_temp_dir] can
                 remain on disk. This is not a problem because we clean the
                 temporary directory later. *)
              Path.rename path_in_temp_dir path_in_build_dir
            with
            | exception e -> Store_result.Error e
            | () -> Already_present)
          | exception e -> Error e
          | () -> Stored
        in
        let store_using_test_and_rename () =
          (* CR-someday amokhov: There is a race here. If [path_in_cache] is
             created after [Path.exists] but before [Path.rename], it will be
             silently overwritten. Find a good way to avoid this race. *)
          match Path.exists path_in_cache with
          | true -> Store_result.Already_present
          | false -> (
            match
              Dune_cache_storage.Util.Optimistically.rename
                ~src:path_in_temp_dir ~dst:path_in_cache
            with
            | exception e -> Error e
            | () -> Stored)
        in
        let result =
          match (mode : Dune_cache_storage.Mode.t) with
          | Hardlink -> store_using_hardlinks ()
          | Copy -> store_using_test_and_rename ()
        in
        Store_result.combine results result)

  let store_skipping_metadata ~mode ~targets ~compute_digest :
      Store_artifacts_result.t Fiber.t =
    Dune_cache_storage.with_temp_dir ~suffix:"artifacts" (function
      | Error exn -> Fiber.return (Store_artifacts_result.Error exn)
      | Ok temp_dir -> (
        match store_targets_to ~temp_dir ~targets ~mode with
        | Error exn -> Fiber.return (Store_artifacts_result.Error exn)
        | Ok () -> (
          compute_digests_in ~temp_dir ~targets ~compute_digest >>| function
          | Error exn -> Store_artifacts_result.Error exn
          | Ok artifacts ->
            let result = store_to_cache_from ~temp_dir ~mode artifacts in
            Store_artifacts_result.of_store_result ~artifacts result)))

  let store ~mode ~rule_digest ~compute_digest targets :
      Store_artifacts_result.t Fiber.t =
    let+ result = store_skipping_metadata ~mode ~targets ~compute_digest in
    Store_artifacts_result.bind result ~f:(fun artifacts ->
        let result = store_metadata ~mode ~rule_digest ~metadata:[] artifacts in
        Store_artifacts_result.of_store_result ~artifacts result)

  let rec fold_list_result l ~init ~f =
    match l with
    | [] -> Ok init
    | x :: xs -> (
      match f init x with
      | Ok acc -> fold_list_result xs ~init:acc ~f
      | Error e -> Error e)

  let create_all_or_nothing ~create ~destroy list =
    fold_list_result list ~init:[] ~f:(fun acc x ->
        match create x with
        | Error e ->
          List.iter acc ~f:destroy;
          Error e
        | Ok v -> Ok (v :: acc))
    |> Result.map ~f:List.rev

  type file_restore_error =
    | Not_found
    | Other of exn

  let restore ~mode ~rule_digest ~target_dir =
    Restore_result.bind (list ~rule_digest)
      ~f:(fun (entries : Metadata_entry.t list) ->
        match
          create_all_or_nothing entries
            ~destroy:(fun (path_in_build_dir, _digest) ->
              Path.Build.unlink_no_err path_in_build_dir)
            ~create:(fun { Metadata_entry.file_name; file_digest } ->
              let path_in_build_dir =
                Path.Build.relative target_dir file_name
              in
              let path_in_cache = file_path ~file_digest in
              match (mode : Dune_cache_storage.Mode.t) with
              | Hardlink -> (
                match
                  link_even_if_there_are_too_many_links_already
                    ~src:path_in_cache
                    ~dst:(Path.build path_in_build_dir)
                with
                | exception Unix.Unix_error (Unix.ENOENT, _, _) ->
                  Error (Not_found : file_restore_error)
                | exception exn -> Error (Other exn)
                | () -> Ok (path_in_build_dir, file_digest))
              | Copy -> (
                match
                  Io.copy_file ~src:path_in_cache
                    ~dst:(Path.build path_in_build_dir)
                    ()
                with
                | exception Sys_error _ -> Error Not_found
                | () -> Ok (path_in_build_dir, file_digest)))
        with
        | Ok artifacts -> Restored artifacts
        | Error Not_found ->
          (* We reach this point when one of the entries mentioned in the
             metadata is missing. The trimmer will eventually delete such
             "broken" metadata, so it is reasonable to consider that this
             [rule_digest] is not found in the cache. *)
          Not_found_in_cache
        | Error (Other e) -> Error e)
end

let store_artifacts = Artifacts.store

let restore_artifacts = Artifacts.restore
