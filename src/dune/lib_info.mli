(** Raw library descriptions *)

(** This module regroup all information about a library. We call such
    descriptions "raw" as the names, such as the names of dependencies are
    plain unresolved library names.

    The [Lib] module takes care of resolving library names to actual libraries. *)

open Stdune

module Status : sig
  type t =
    | Installed
    | Public of Dune_project.Name.t * Package.t
    | Private of Dune_project.t

  val pp : t Fmt.t

  val is_private : t -> bool

  (** For local libraries, return the project name they are part of *)
  val project_name : t -> Dune_project.Name.t option
end

(** For values like modules that need to be evaluated to be fetched *)
module Source : sig
  type 'a t =
    | Local
    | External of 'a
end

module Enabled_status : sig
  type t =
    | Normal
    | Optional
    | Disabled_because_of_enabled_if
end

module Special_builtin_support : sig
  module Build_info : sig
    type api_version = V1

    type t =
      { data_module : string
      ; api_version : api_version
      }
  end

  type t =
    | Findlib_dynload
    | Build_info of Build_info.t

  include Dune_lang.Conv.S with type t := t
end

module Inherited : sig
  type 'a t =
    | This of 'a
    | From of (Loc.t * Lib_name.t)
end

module Main_module_name : sig
  type t = Module_name.t option Inherited.t
end

module Shared : sig
  type t

  val create :
       synopsis:string option
    -> kind:Lib_kind.t
    -> variant:(Loc.t * Variant.t) option
    -> default_implementation:(Loc.t * Lib_name.t) option
    -> special_builtin_support:Special_builtin_support.t option
    -> implements:(Loc.t * Lib_name.t) option
    -> virtual_deps:(Loc.t * Lib_name.t) list
    -> ppx_runtime_deps:(Loc.t * Lib_name.t) list
    -> t

  val fields : dune_file:bool -> t Dune_lang.Decoder.fields_parser

  val implements : t -> (Loc.t * Lib_name.t) option

  val variant : t -> (Loc.t * Variant.t) option

  val default_implementation : t -> (Loc.t * Lib_name.t) option

  val special_builtin_support : t -> Special_builtin_support.t option

  val kind : t -> Lib_kind.t
end

type 'path t

val shared : _ t -> Shared.t

val name : _ t -> Lib_name.t

val loc : _ t -> Loc.t

val archives : 'path t -> 'path list Mode.Dict.t

val foreign_archives : 'path t -> 'path list Mode.Dict.t

val foreign_objects : 'path t -> 'path list Source.t

val plugins : 'path t -> 'path list Mode.Dict.t

val src_dir : 'path t -> 'path

val status : _ t -> Status.t

val variant : 'a t -> (Loc.t * Variant.t) option

val default_implementation : _ t -> (Loc.t * Lib_name.t) option

val kind : _ t -> Lib_kind.t

val synopsis : _ t -> string option

val jsoo_runtime : 'path t -> 'path list

val jsoo_archive : 'path t -> 'path option

val obj_dir : 'path t -> 'path Obj_dir.t

val virtual_ : _ t -> Modules.t Source.t option

val main_module_name : _ t -> Main_module_name.t

val wrapped : _ t -> Wrapped.t Inherited.t option

val special_builtin_support : _ t -> Special_builtin_support.t option

val modes : _ t -> Mode.Dict.Set.t

val implements : _ t -> (Loc.t * Lib_name.t) option

val known_implementations : _ t -> (Loc.t * Lib_name.t) Variant.Map.t

val requires : _ t -> Lib_dep.t list

val ppx_runtime_deps : _ t -> (Loc.t * Lib_name.t) list

val pps : _ t -> (Loc.t * Lib_name.t) list

val sub_systems : _ t -> Sub_system_info.t Sub_system_name.Map.t

val enabled : _ t -> Enabled_status.t

val orig_src_dir : 'path t -> 'path option

val version : _ t -> string option

(** Directory where the source files for the library are located. Returns the
    original src dir when it exists *)
val best_src_dir : 'path t -> 'path

type external_ = Path.t t

type local = Path.Build.t t

val user_written_deps : _ t -> Lib_dep.t list

val of_local : local -> external_

val as_local_exn : external_ -> local

val set_version : 'a t -> string option -> 'a t

val for_dune_package :
     Path.t t
  -> ppx_runtime_deps:(Loc.t * Lib_name.t) list
  -> requires:Lib_dep.t list
  -> foreign_objects:Path.t list
  -> obj_dir:Path.t Obj_dir.t
  -> implements:(Loc.t * Lib_name.t) option
  -> default_implementation:(Loc.t * Lib_name.t) option
  -> sub_systems:Sub_system_info.t Sub_system_name.Map.t
  -> Path.t t

val map_path : 'a t -> f:('a -> 'a) -> 'a t

val create :
     loc:Loc.t
  -> name:Lib_name.t
  -> shared:Shared.t
  -> status:Status.t
  -> src_dir:'a
  -> orig_src_dir:'a option
  -> obj_dir:'a Obj_dir.t
  -> version:string option
  -> main_module_name:Main_module_name.t
  -> sub_systems:Sub_system_info.t Sub_system_name.Map.t
  -> requires:Lib_dep.t list
  -> foreign_objects:'a list Source.t
  -> plugins:'a list Mode.Dict.t
  -> archives:'a list Mode.Dict.t
  -> foreign_archives:'a list Mode.Dict.t
  -> jsoo_runtime:'a list
  -> jsoo_archive:'a option
  -> pps:(Loc.t * Lib_name.t) list
  -> enabled:Enabled_status.t
  -> dune_version:Dune_lang.Syntax.Version.t option
  -> virtual_:Modules.t Source.t option
  -> known_implementations:(Loc.t * Lib_name.t) Variant.Map.t
  -> modes:Mode.Dict.Set.t
  -> wrapped:Wrapped.t Inherited.t option
  -> 'a t
