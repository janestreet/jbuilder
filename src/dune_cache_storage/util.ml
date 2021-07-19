open Stdune

module Optimistically = struct
  let rename ~src ~dst =
    try Path.rename src dst with
    | Sys_error _ ->
      Path.mkdir_p (Path.parent_exn dst);
      Path.rename src dst

  let link ~src ~dst =
    try Path.link src dst with
    | Unix.Unix_error (Unix.ENOENT, _, _) ->
      Path.mkdir_p (Path.parent_exn dst);
      Path.link src dst
end

let link_or_copy ~mode ~src ~dst =
  match (mode : Mode.t) with
  | Hardlink -> Path.link src dst
  | Copy -> Io.copy_file ~src ~dst ()

module Write_result = struct
  type t =
    | Ok
    | Already_present
    | Error of exn
end

let add_atomically ~mode ~src ~dst : Write_result.t =
  match (mode : Mode.t) with
  | Hardlink -> (
    match Optimistically.link ~src ~dst with
    | () -> Ok
    | exception Unix.Unix_error (Unix.EEXIST, _, _) -> Already_present
    | exception e -> Error e)
  | Copy -> (
    (* CR-someday amokhov: There is a race here. If the destination [dst] is
       created after [Path.exists] but before [Path.rename], [dst] will be
       silently overwritten. Find a good way to avoid this race. *)
    match Path.exists dst with
    | true -> Already_present
    | false -> (
      match Optimistically.rename ~src ~dst with
      | () -> Ok
      | exception e -> Error e))

(* CR-someday amokhov: Switch to [renameat2] to go from two operations to
   one. *)
let write_atomically ~mode ~content dst : Write_result.t =
  Temp.with_temp_file ~dir:Layout.temp_dir ~prefix:"dune" ~suffix:"write"
    ~f:(function
    | Error e -> Write_result.Error e
    | Ok temp_file -> (
      match Io.write_file ~binary:false temp_file content with
      | exception e -> Error e
      | () -> add_atomically ~mode ~src:temp_file ~dst))
