open! Stdune
open Import
open Dune_lang.Decoder

module Build_flags_resolver = struct

  module Vendored = struct
    type t =
      { c_flags : Ordered_set_lang.Unexpanded.t
      ; c_library_flags : Ordered_set_lang.Unexpanded.t }

    let decode =
      fields
        (let+ c_flags = Ordered_set_lang.Unexpanded.field "c_flags"
          and+ c_library_flags = Ordered_set_lang.Unexpanded.field "c_library_flags" in
          { c_flags; c_library_flags })
  end

  type t =
    | Pkg_config
    | Vendored of Vendored.t

  let decode =
    let vendored =
      let+ p = Vendored.decode in
      Vendored p
    in
    sum [ ("pkg_config" , return Pkg_config)
        ; ("vendored"   , vendored ) ]

  let default = Pkg_config
end

module Concurrency_policy = struct
  type t =
    | Sequential
    | Unlocked
    | Lwt_jobs
    | Lwt_preemptive

  let decode =
    enum [ "sequential"     , Sequential
          ; "unlocked"       , Unlocked
          ; "lwt_jobs"       , Lwt_jobs
          ; "lwt_preemptive" , Lwt_preemptive ]

  let default = Sequential
end

module Headers = struct
  type t =
    | Include of string list
    | Preamble of string

  let decode =
    let include_ =
      let+ s = repeat string in
      Include s
    in
    let preamble =
      let+ p = string in
      Preamble p
    in
    sum [ ("include"  , include_)
        ; ("preamble" , preamble) ]

  let default = Include []
end

module Type_description = struct
  type t =
    { functor_ : Module_name.t
    ; instance : Module_name.t }

  let decode =
    let open Dune_lang.Decoder in
    fields
      (let+ functor_ = field "functor" Module_name.decode
        and+ instance =  field "instance" Module_name.decode
        in
        { functor_; instance })
end

module Function_description = struct
  type t =
    { concurrency : Concurrency_policy.t
    ; functor_ : Module_name.t
    ; instance : Module_name.t }

  let decode =
    let open Dune_lang.Decoder in
    fields
      (let+ concurrency = field_o "concurrency" Concurrency_policy.decode
        and+ functor_ = field "functor" Module_name.decode
        and+ instance =  field "instance" Module_name.decode
        in
        { concurrency = Option.value concurrency ~default:Concurrency_policy.default
        ; functor_
        ; instance })
end

type t =
  { external_library_name : string
  ; build_flags_resolver : Build_flags_resolver.t
  ; headers : Headers.t
  ; type_description : Type_description.t
  ; function_description : Function_description.t list
  ; generated_types       : Module_name.t
  ; generated_entry_point : Module_name.t }

let name = "ctypes"

type Stanza.t += T of t

let syntax =
  Dune_lang.Syntax.create ~name ~desc:"the ctypes extension"
    [ ((0, 1), `Since (3, 0)) ]

let decode =
  let open Dune_lang.Decoder in
  fields
    (let+ external_library_name = field "external_library_name" string
      and+ build_flags_resolver = field_o "build_flags_resolver" Build_flags_resolver.decode
      and+ type_description = field "type_description" Type_description.decode
      and+ function_description = multi_field "function_description" Function_description.decode
      and+ headers = field_o "headers" Headers.decode
      and+ generated_types       = field_o "generated_types" Module_name.decode
      and+ generated_entry_point = field "generated_entry_point" Module_name.decode
    in
    { external_library_name
    ; build_flags_resolver = Option.value build_flags_resolver ~default:Build_flags_resolver.default
    ; headers = Option.value headers ~default:Headers.default
    ; type_description
    ; function_description
    ; generated_types = Option.value generated_types ~default:(Module_name.of_string "Types_generated")
    ; generated_entry_point })

let () =
  let open Dune_lang.Decoder in
  Dune_project.Extension.register_simple syntax
    (return [ (name, decode >>| fun x -> [ T x ]) ])
