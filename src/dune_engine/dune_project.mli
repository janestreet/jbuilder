(** dune-project files *)
open! Stdune

open Import

module Name : sig
  (** Invariants: - Named s -> s <> "" and s does not contain '.' or '/' -
      Anonymous p -> p is a local path in the source tree *)
  type t = private
    | Named of string
    | Anonymous of Path.Source.t

  val to_dyn : t -> Dyn.t

  val equal : t -> t -> bool

  val compare : t -> t -> Ordering.t

  (** Convert to a string that is suitable for human readable messages *)
  val to_string_hum : t -> string

  (** Convert to/from an encoded string that is suitable to use in filenames *)
  val to_encoded_string : t -> string

  val of_encoded_string : string -> t

  module Infix : Comparator.OPS with type t = t

  module Map : Map.S with type key = t
end

type t

module File_key : sig
  (** File_key encodes the project in a unique way to be used as part of file
      path. *)
  type t

  val to_string : t -> string

  val of_string : string -> t

  module Map : Map.S with type key = t
end

val to_dyn : t -> Dyn.t

val file_key : t -> File_key.t

val packages : t -> Package.t Package.Name.Map.t

val version : t -> string option

val name : t -> Name.t

val root : t -> Path.Source.t

val stanza_parser : t -> Stanza.t list Dune_lang.Decoder.t

val generate_opam_files : t -> bool

(** The option [use_standard_c_and_cxx_flags] enables the automatic addition of
    flags necessary to build c++ files with the active C compiler. It also
    disables the automatic addition of C flags from [ocamlc -config] to the
    compiler command line when building C stubs. *)
val use_standard_c_and_cxx_flags : t -> bool option

val dialects : t -> Dialect.DB.t

val explicit_js_mode : t -> bool

val format_config : t -> Format_config.t

val equal : t -> t -> bool

val hash : t -> int

val parsing_context : t -> Univ_map.t

(** Return the path of the project file. *)
val file : t -> Path.Source.t

module Lang : sig
  (** [register id stanzas_parser] register a new language. Users will select
      this language by writing:

      {[ (lang <name> <version>) ]}

      as the first line of their [dune-project] file. [stanza_parsers] defines
      what stanzas the user can write in [dune] files. *)
  val register : Dune_lang.Syntax.t -> Stanza.Parser.t list -> unit
end

module Extension : sig
  type 'a t

  (** [register id parser] registers a new extension. Users will enable this
      extension by writing:

      {[ (using <name> <version> <args>) ]}

      in their [dune-project] file. [parser] is used to describe what [<args>]
      might be. *)
  val register :
       Dune_lang.Syntax.t
    -> ('a * Stanza.Parser.t list) Dune_lang.Decoder.t
    -> ('a -> Dyn.t)
    -> 'a t

  (** A simple version where the arguments are not used through
      [find_extension_args]. *)
  val register_simple :
    Dune_lang.Syntax.t -> Stanza.Parser.t list Dune_lang.Decoder.t -> unit

  (** Register experimental extensions that were deleted *)
  val register_deleted :
    name:string -> deleted_in:Dune_lang.Syntax.Version.t -> unit
end

(** Load a project description from the following directory. [files] is the set
    of files in this directory.

    If [infer_from_opam_files] is true and the directory contains no
    [dune-project] file but contains at least one [>package>.opam] files, then a
    project description is inferred from the opam files. *)
val load :
     dir:Path.Source.t
  -> files:String.Set.t
  -> infer_from_opam_files:bool
  -> dir_status:Sub_dirs.Status.t
  -> t option

(** Create an anonymous project with no package rooted at the given directory *)
val anonymous : dir:Path.Source.t -> t

(** "dune-project" *)
val filename : string

type created_or_already_exist =
  | Created
  | Already_exist

(** Default language version to use for projects that don't have a
    [dune-project] file. The default value is the latest version of the dune
    language. *)
val default_dune_language_version : Dune_lang.Syntax.Version.t ref

(** Set the project we are currently parsing dune files for *)
val set :
  t -> ('a, 'k) Dune_lang.Decoder.parser -> ('a, 'k) Dune_lang.Decoder.parser

val get_exn : unit -> (t, 'k) Dune_lang.Decoder.parser

(** Find arguments passed to (using). [None] means that the extension was not
    written in dune-project. *)
val find_extension_args : t -> 'a Extension.t -> 'a option

val set_parsing_context : t -> 'a Dune_lang.Decoder.t -> 'a Dune_lang.Decoder.t

val implicit_transitive_deps : t -> bool

val dune_version : t -> Dune_lang.Syntax.Version.t

val wrapped_executables : t -> bool

val executables_implicit_empty_intf : t -> bool

val accept_alternative_dune_file_name : t -> bool

val strict_package_deps : t -> bool

val cram : t -> bool

val info : t -> Package.Info.t

(** Update the execution parameters according to what is written in the
    [dune-project] file. *)
val update_execution_parameters :
  t -> Execution_parameters.t -> Execution_parameters.t
