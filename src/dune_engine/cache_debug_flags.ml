(** For each cache layer, the flag controls if dune should, on each cache miss,
    print an explanation of why lookup failed. *)
type t =
  { shared_cache : bool
  ; workspace_local_cache : bool (* TODO: digest_cache : bool *)
  }
