open! Dune_engine
open! Stdune

(* This module would be part of Ctypes_rules, except it creates a circular
   dependency if Dune_file tries to access it. *)

val cflags_sexp : external_library_name:string -> string

val c_library_flags : external_library_name:string -> string

val c_generated_functions_cout_no_ext :
     external_library_name:string
  -> functor_:Module_name.t
  -> instance:Module_name.t
  -> string

val libraries_needed_for_ctypes : loc:Loc.t -> Lib_dep.t list

val add :
     loc:Loc.t
  -> parsing_context:Univ_map.t
  -> external_library_name:string
  -> add_stubs:(Foreign_language.t
                -> loc:Loc.t
                -> names:Ordered_set_lang.t option
                -> flags:Ordered_set_lang.Unexpanded.t option
                -> Foreign.Stubs.t list
                -> Foreign.Stubs.t list)
  -> functor_:Module_name.t
  -> instance:Module_name.t
  -> foreign_stubs:Foreign.Stubs.t list
  -> Foreign.Stubs.t list
