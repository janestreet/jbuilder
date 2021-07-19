module Caml_digest = Digest
open! Stdune
open Import

let digest_string string =
  let digest = Digest.Direct_impl.string string in
  (match
     (* CR-someday amokhov: Below we do not respect the [cache_storage_mode]
        configuration setting. This will break if hard links are not
        supported. *)
     Dune_cache_storage.Raw_value.store_unchecked ~content:string
       ~content_digest:digest ~mode:Hardlink
   with
  | Ok
  | Already_present ->
    ()
  | Error exn ->
    Log.info
      [ Pp.textf "error making digest reversible [%s]: %s"
          (Stdune.Digest.to_string digest)
          (Dyn.to_string (Exn.to_dyn exn))
      ]);
  digest

let digest_file file =
  let contents = Io.String_path.read_file ~binary:true file in
  digest_string contents

let enable () = Digest.override_impl ~file:digest_file ~string:digest_string
