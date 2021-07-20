open! Stdune
open Import

module Build_flags_resolver : sig
  module Vendored : sig
    type t =
      { c_flags : Ordered_set_lang.Unexpanded.t
      ; c_library_flags : Ordered_set_lang.Unexpanded.t }
  end
  type t =
    | Pkg_config
    | Vendored of Vendored.t
end

module Concurrency_policy : sig
  type t =
    | Sequential
    | Unlocked
    | Lwt_jobs
    | Lwt_preemptive
end

module Headers : sig
  type t =
    | Include of string list
    | Preamble of string
end

module Type_description : sig
  type t =
    { functor_ : Module_name.t
    ; instance : Module_name.t }
end

module Function_description : sig
  type t =
    { concurrency : Concurrency_policy.t
    ; functor_ : Module_name.t
    ; instance : Module_name.t }
end

type t =
  { external_library_name : string
  ; build_flags_resolver : Build_flags_resolver.t
  ; headers : Headers.t
  ; type_description : Type_description.t
  ; function_description : Function_description.t list
  ; generated_types : Module_name.t
  ; generated_entry_point : Module_name.t }

type Stanza.t += T of t

val decode : t Dune_lang.Decoder.t

val syntax : Dune_lang.Syntax.t
