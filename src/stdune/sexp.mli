type t = Sexp0.t =
  | Atom of string
  | List of t list

val to_string : t -> string

val pp : Format.formatter -> t -> unit

val hash : t -> int

val equal : t -> t -> bool

val compare : t -> t -> Ordering.t
