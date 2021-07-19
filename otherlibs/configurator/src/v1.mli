type t

val create :
     ?dest_dir:string
  -> ?ocamlc:string
  -> ?log:(string -> unit)
  -> string (** name, such as library name *)
  -> t

(** Return the value associated to a variable in the output of [ocamlc -config] *)
val ocaml_config_var : t -> string -> string option

val ocaml_config_var_exn : t -> string -> string

(** [c_test t ?c_flags ?link_flags c_code] try to compile and link the C code
    given in [c_code]. Return whether compilation was successful. *)
val c_test :
     t
  -> ?c_flags:string list (** default: [] *)
  -> ?link_flags:string list (** default: [] *)
  -> string
  -> bool

module C_define : sig
  module Type : sig
    type t =
      | Switch  (** defined/undefined *)
      | Int
      | String
  end

  module Value : sig
    type t =
      | Switch of bool
      | Int of int
      | String of string
  end

  (** Import some #define from the given header files. For instance:

      {v
        # C.C_define.import c ~includes:"caml/config.h" ["ARCH_SIXTYFOUR", Switch];;
        - (string * Configurator.C_define.Value.t) list = ["ARCH_SIXTYFOUR", Switch true]
      v} *)
  val import :
       t
    -> ?prelude:string
         (** Define extra code be used with extracting values below. Note that
             the compiled code is never executed. *)
    -> ?c_flags:string list
    -> includes:string list
    -> (string * Type.t) list
    -> (string * Value.t) list

  (** Generate a C header file containing the following #define.
      [protection_var] is used to enclose the file with:

      {[ #ifndef BLAH #define BLAH ... #endif ]}

      If not specified, it is inferred from the name given to [create] and the
      filename. *)
  val gen_header_file :
       t
    -> fname:string
    -> ?protection_var:string
    -> (string * Value.t) list
    -> unit
end

module Pkg_config : sig
  type configurator = t

  type t

  (** Search pkg-config in the PATH. Returns [None] if pkg-config is not found. *)
  val get : configurator -> t option

  type package_conf =
    { libs : string list
    ; cflags : string list
    }

  (** [query t ~package] query pkg-config for the [package]. The package must
      not contain a version constraint. Multiple, unversioned packages are
      separated with spaces, for example "gtk+-3.0 gtksourceview-3.0". Returns
      [None] if [package] is not available *)
  val query : t -> package:string -> package_conf option

  val query_expr : t -> package:string -> expr:string -> package_conf option
    [@@ocaml.deprecated "please use [query_expr_err]"]

  (** [query_expr_err t ~package ~expr] query pkg-config for the [package].
      [expr] may contain a version constraint, for example "gtk+-3.0 >= 3.18".
      [package] must be just the name of the package. If [expr] is specified,
      [package] must be specified as well. Returns [Error error_msg] if
      [package] is not available *)
  val query_expr_err :
    t -> package:string -> expr:string -> (package_conf, string) result
end
with type configurator := t

module Flags : sig
  (** [write_sexp fname s] writes the list of strings [s] to the file [fname] in
      an appropriate format so that it can used in [dune] files with
      [(:include \[fname\])]. *)
  val write_sexp : string -> string list -> unit

  (** [write_lines fname s] writes the list of string [s] to the file [fname]
      with one line per string so that it can be used in Dune action rules with
      [%{read-lines:<path>}]. *)
  val write_lines : string -> string list -> unit

  (** [extract_comma_space_separated_words s] returns a list of words in [s]
      that are separated by a newline, tab, space or comma character. *)
  val extract_comma_space_separated_words : string -> string list

  (** [extract_blank_separated_words s] returns a list of words in [s] that are
      separated by a tab or space character. *)
  val extract_blank_separated_words : string -> string list

  (** [extract_words s ~is_word_char] will split the string [s] into a list of
      words. A valid word character is defined by the [is_word_char] predicate
      returning true and anything else is considered a separator. Any blank
      words are filtered out of the results. *)
  val extract_words : string -> is_word_char:(char -> bool) -> string list
end

(** [which t prog] seek [prog] in the PATH and return the name of the program
    prefixed with the first path where it is found. Return [None] the the
    program is not found. *)
val which : t -> string -> string option

(** Execute external programs. *)
module Process : sig
  type result =
    { exit_code : int
    ; stdout : string
    ; stderr : string
    }

  (** [run t prog args] runs [prog] with arguments [args] and returns its exit
      status together with the content of stdout and stderr. The action is
      logged.

      @param dir change to [dir] before running the command.
      @param env
        specify additional environment variables as a list of the form
        NAME=VALUE. *)
  val run :
    t -> ?dir:string -> ?env:string list -> string -> string list -> result

  (** [run_capture_exn t prog args] same as [run t prog args] but returns
      [stdout] and {!die} if the error code is nonzero or there is some output
      on [stderr]. *)
  val run_capture_exn :
    t -> ?dir:string -> ?env:string list -> string -> string list -> string

  (** [run_ok t prog args] same as [run t prog args] but only cares whether the
      execution terminated successfully (i.e., returned an error code of [0]). *)
  val run_ok :
    t -> ?dir:string -> ?env:string list -> string -> string list -> bool
end

(** Typical entry point for configurator programs *)
val main :
     ?args:(Arg.key * Arg.spec * Arg.doc) list
  -> name:string
  -> (t -> unit)
  -> unit

(** Abort execution. If raised from within [main], the argument of [die] is
    printed as [Error: <message>]. *)
val die : ('a, unit, string, 'b) format4 -> 'a
