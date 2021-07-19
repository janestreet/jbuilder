open Stdune

type t =
  | Not_supported
  | Zero_terminated_strings of string

(* This mutable table is safe under the assumption that a program path always
   points to the binary with the same version. While the assumption seems likely
   to hold, it would be better to avoid the need for it to simplify
   reasoning. *)
let registry = Table.create (module Path) 128

let get ~prog = Option.value (Table.find registry prog) ~default:Not_supported

let set ~prog t = Table.set registry prog t
