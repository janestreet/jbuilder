open! Dune_engine
open! Stdune
open Dune_lang.Decoder

(* workspace files use the same version numbers as dune-project files for
   simplicity *)
let syntax = Stanza.syntax

let env_field, env_field_lazy =
  let make f g =
    field "env" ~default:(f Dune_env.Stanza.empty)
      (g (Dune_lang.Syntax.since syntax (1, 1) >>> Dune_env.Stanza.decode))
  in
  (make Fun.id Fun.id, make Lazy.from_val lazy_)

module Context = struct
  module Target = struct
    type t =
      | Native
      | Named of Context_name.t

    let equal x y =
      match (x, y) with
      | Native, Native -> true
      | Native, _
      | _, Native ->
        false
      | Named x, Named y -> Context_name.equal x y

    let t =
      let+ context_name = Context_name.decode in
      match Context_name.to_string context_name with
      | "native" -> Native
      | _ -> Named context_name

    let add ts x =
      match x with
      | None -> ts
      | Some t ->
        if List.mem ts t ~equal then
          ts
        else
          ts @ [ t ]
  end

  module Common = struct
    type t =
      { loc : Loc.t
      ; profile : Profile.t
      ; targets : Target.t list
      ; env : Dune_env.Stanza.t
      ; toolchain : Context_name.t option
      ; name : Context_name.t
      ; host_context : Context_name.t option
      ; paths : (string * Ordered_set_lang.t) list
      ; fdo_target_exe : Path.t option
      ; dynamically_linked_foreign_archives : bool
      ; instrument_with : Lib_name.t list
      ; merlin : bool
      }

    let to_dyn = Dyn.Encoder.opaque

    let equal
        { loc = _
        ; profile
        ; targets
        ; env
        ; toolchain
        ; name
        ; host_context
        ; paths
        ; fdo_target_exe
        ; dynamically_linked_foreign_archives
        ; instrument_with
        ; merlin
        } t =
      Profile.equal profile t.profile
      && List.equal Target.equal targets t.targets
      && Dune_env.Stanza.equal env t.env
      && Option.equal Context_name.equal toolchain t.toolchain
      && Context_name.equal name t.name
      && Option.equal Context_name.equal host_context t.host_context
      && List.equal
           (Tuple.T2.equal String.equal Ordered_set_lang.equal)
           paths t.paths
      && Option.equal Path.equal fdo_target_exe t.fdo_target_exe
      && Bool.equal dynamically_linked_foreign_archives
           t.dynamically_linked_foreign_archives
      && List.equal Lib_name.equal instrument_with t.instrument_with
      && Bool.equal merlin t.merlin

    let fdo_suffix t =
      match t.fdo_target_exe with
      | None -> ""
      | Some file ->
        let name, _ = Path.split_extension file in
        "-fdo-" ^ Path.basename name

    let t =
      let+ env = env_field
      and+ targets =
        field "targets" (repeat Target.t) ~default:[ Target.Native ]
      and+ profile = field_o "profile" Profile.decode
      and+ host_context =
        field_o "host"
          (Dune_lang.Syntax.since syntax (1, 10) >>> Context_name.decode)
      and+ toolchain =
        field_o "toolchain"
          (Dune_lang.Syntax.since syntax (1, 5) >>> Context_name.decode)
      and+ dynamically_linked_foreign_archives =
        let+ disable =
          field ~default:false "disable_dynamically_linked_foreign_archives"
            (Dune_lang.Syntax.since syntax (2, 0) >>> bool)
        in
        not disable
      and+ fdo_target_exe =
        let f file =
          let ext = Filename.extension file in
          if ext = ".exe" then
            Path.(relative root file)
          else
            User_error.raise
              [ Pp.textf
                  "`fdo %s` expects executable filename ending with .exe \
                   extension, not %s. \n\
                   Please specify the name of the executable to optimize, \
                   including path from <root>."
                  file ext
              ]
        in
        field_o "fdo" (Dune_lang.Syntax.since syntax (2, 0) >>> map string ~f)
      and+ paths =
        let f l =
          match
            Env.Map.of_list (List.map ~f:(fun ((loc, s), _) -> (s, loc)) l)
          with
          | Ok _ -> List.map ~f:(fun ((_, s), x) -> (s, x)) l
          | Error (var, _, loc) ->
            User_error.raise ~loc
              [ Pp.textf
                  "the variable %S can appear at most once in this stanza." var
              ]
        in
        field "paths" ~default:[]
          (Dune_lang.Syntax.since Stanza.syntax (1, 12)
          >>> map ~f (repeat (pair (located string) Ordered_set_lang.decode)))
      and+ instrument_with =
        field_o "instrument_with"
          (Dune_lang.Syntax.since syntax (2, 7) >>> repeat Lib_name.decode)
      and+ loc = loc
      and+ merlin = field_b "merlin" in
      fun ~profile_default ~instrument_with_default ->
        let profile = Option.value profile ~default:profile_default in
        let instrument_with =
          Option.value instrument_with ~default:instrument_with_default
        in
        Option.iter host_context ~f:(fun _ ->
            match targets with
            | [ Target.Native ] -> ()
            | _ ->
              User_error.raise ~loc
                [ Pp.text
                    "`targets` and `host` options cannot be used in the same \
                     context."
                ]);
        { targets
        ; profile
        ; loc
        ; env
        ; name = Context_name.default
        ; host_context
        ; toolchain
        ; paths
        ; fdo_target_exe
        ; dynamically_linked_foreign_archives
        ; instrument_with
        ; merlin
        }
  end

  module Opam = struct
    type t =
      { base : Common.t
      ; switch : string
      ; root : string option
      }

    let to_dyn { base; switch; root } =
      let open Dyn.Encoder in
      record
        [ ("base", Common.to_dyn base)
        ; ("switch", string switch)
        ; ("root", option string root)
        ]

    let equal { base; switch; root } t =
      Common.equal base t.base
      && String.equal switch t.switch
      && Option.equal String.equal root t.root

    let t =
      let+ loc_switch, switch = field "switch" (located string)
      and+ name = field_o "name" Context_name.decode
      and+ root = field_o "root" string
      and+ base = Common.t in
      fun ~profile_default ~instrument_with_default ~x ->
        let base = base ~profile_default ~instrument_with_default in
        let name =
          match name with
          | Some s -> s
          | None -> (
            let name = switch ^ Common.fdo_suffix base in
            match Context_name.of_string_opt name with
            | Some s -> s
            | None ->
              User_error.raise ~loc:loc_switch
                [ Pp.textf "Generated context name %S is invalid" name
                ; Pp.text
                    "Please specify a context name manually with the (name ..) \
                     field"
                ])
        in
        let base = { base with targets = Target.add base.targets x; name } in
        { base; switch; root }
  end

  module Default = struct
    type t = Common.t

    let to_dyn = Common.to_dyn

    let t =
      let+ common = Common.t
      and+ name =
        field_o "name"
          ( Dune_lang.Syntax.since syntax (1, 10) >>= fun () ->
            Context_name.decode )
      in
      fun ~profile_default ~instrument_with_default ~x ->
        let common = common ~profile_default ~instrument_with_default in
        let default =
          (* TODO proper error handling with locs *)
          let name =
            Context_name.to_string common.name ^ Common.fdo_suffix common
          in
          Context_name.parse_string_exn (Loc.none, name)
        in
        let name = Option.value ~default name in
        { common with targets = Target.add common.targets x; name }

    let equal = Common.equal
  end

  type t =
    | Default of Default.t
    | Opam of Opam.t

  let hash = Hashtbl.hash

  let to_dyn =
    let open Dyn.Encoder in
    function
    | Default d -> constr "Default" [ Default.to_dyn d ]
    | Opam o -> constr "Opam" [ Opam.to_dyn o ]

  let equal x y =
    match (x, y) with
    | Default x, Default y -> Default.equal x y
    | Opam x, Opam y -> Opam.equal x y
    | _, _ -> false

  let loc = function
    | Default x -> x.loc
    | Opam x -> x.base.loc

  let host_context = function
    | Default { host_context; _ }
    | Opam { base = { host_context; _ }; _ } ->
      host_context

  let t =
    sum
      [ ( "default"
        , let+ f = fields Default.t in
          fun ~profile_default ~instrument_with_default ~x ->
            Default (f ~profile_default ~instrument_with_default ~x) )
      ; ( "opam"
        , let+ f = fields Opam.t in
          fun ~profile_default ~instrument_with_default ~x ->
            Opam (f ~profile_default ~instrument_with_default ~x) )
      ]

  let env = function
    | Default d -> d.env
    | Opam o -> o.base.env

  let name = function
    | Default d -> d.name
    | Opam o -> o.base.name

  let targets = function
    | Default x -> x.targets
    | Opam x -> x.base.targets

  let all_names t =
    let n = name t in
    n
    :: List.filter_map (targets t) ~f:(function
         | Native -> None
         | Named s -> Some (Context_name.target n ~toolchain:s))

  let default ~x ~profile ~instrument_with =
    Default
      { loc = Loc.of_pos __POS__
      ; targets = [ Option.value x ~default:Target.Native ]
      ; profile = Option.value profile ~default:Profile.default
      ; name = Context_name.default
      ; host_context = None
      ; env = Dune_env.Stanza.empty
      ; toolchain = None
      ; paths = []
      ; fdo_target_exe = None
      ; dynamically_linked_foreign_archives = true
      ; instrument_with = Option.value instrument_with ~default:[]
      ; merlin = false
      }

  let build_contexts t =
    let name = name t in
    let native = Build_context.create ~name ~host:(host_context t) in
    native
    :: List.filter_map (targets t) ~f:(function
         | Native -> None
         | Named toolchain ->
           let name = Context_name.target name ~toolchain in
           Some (Build_context.create ~name ~host:(Some native.name)))
end

type t =
  { merlin_context : Context_name.t option
  ; contexts : Context.t list
  ; env : Dune_env.Stanza.t
  ; config : Dune_config.t
  }

let to_dyn { merlin_context; contexts; env; config } =
  let open Dyn.Encoder in
  record
    [ ("merlin_context", option Context_name.to_dyn merlin_context)
    ; ("contexts", list Context.to_dyn contexts)
    ; ("env", Dune_env.Stanza.to_dyn env)
    ; ("config", Dune_config.to_dyn config)
    ]

let equal { merlin_context; contexts; env; config } w =
  Option.equal Context_name.equal merlin_context w.merlin_context
  && List.equal Context.equal contexts w.contexts
  && Dune_env.Stanza.equal env w.env
  && Dune_config.equal config w.config

let hash { merlin_context; contexts; env; config } =
  Hashtbl.hash
    ( Option.hash Context_name.hash merlin_context
    , List.hash Context.hash contexts
    , Dune_env.Stanza.hash env
    , Dune_config.hash config )

include Dune_lang.Versioned_file.Make (struct
  type t = unit
end)

let () = Lang.register syntax ()

module Clflags = struct
  type t =
    { x : Context_name.t option
    ; profile : Profile.t option
    ; instrument_with : Lib_name.t list option
    ; workspace_file : Path.t option
    ; config_from_command_line : Dune_config.Partial.t
    ; config_from_config_file : Dune_config.Partial.t
    }

  let to_dyn
      { x
      ; profile
      ; instrument_with
      ; workspace_file
      ; config_from_command_line
      ; config_from_config_file
      } =
    let open Dyn.Encoder in
    record
      [ ("x", option Context_name.to_dyn x)
      ; ("profile", option Profile.to_dyn profile)
      ; ("instrument_with", option (list Lib_name.to_dyn) instrument_with)
      ; ("workspace_file", option Path.to_dyn workspace_file)
      ; ( "config_from_command_line"
        , Dune_config.Partial.to_dyn config_from_command_line )
      ; ( "config_from_config_file"
        , Dune_config.Partial.to_dyn config_from_config_file )
      ]

  let t = Fdecl.create to_dyn

  let set v = Fdecl.set t v

  let t () = Fdecl.get t
end

let bad_configuration_check map =
  let find_exn loc name host =
    match Context_name.Map.find map host with
    | Some host_ctx -> host_ctx
    | None ->
      User_error.raise ~loc
        [ Pp.textf "Undefined host context '%s' for '%s'."
            (Context_name.to_string host)
            (Context_name.to_string name)
        ]
  in
  let check elt =
    Context.host_context elt
    |> Option.iter ~f:(fun host ->
           let name = Context.name elt in
           let loc = Context.loc elt in
           let host_elt = find_exn loc name host in
           Context.host_context host_elt
           |> Option.iter ~f:(fun host_of_host ->
                  User_error.raise ~loc:(Context.loc host_elt)
                    [ Pp.textf
                        "Context '%s' is both a host (for '%s') and a target \
                         (for '%s')."
                        (Context_name.to_string host)
                        (Context_name.to_string name)
                        (Context_name.to_string host_of_host)
                    ]))
  in
  Context_name.Map.iter map ~f:check

let top_sort contexts =
  let key = Context.name in
  let map =
    Context_name.Map.of_list_map_exn contexts ~f:(fun x -> (key x, x))
  in
  let deps def =
    match Context.host_context def with
    | None -> []
    | Some ctx -> [ Context_name.Map.find_exn map ctx ]
  in
  bad_configuration_check map;
  match Context_name.Top_closure.top_closure ~key ~deps contexts with
  | Ok topo_contexts -> topo_contexts
  | Error _ -> assert false

let create_final_config ~config_from_config_file ~config_from_command_line
    ~config_from_workspace_file =
  let ( ++ ) = Dune_config.superpose in
  Dune_config.default ++ config_from_config_file ++ config_from_workspace_file
  ++ config_from_command_line

(* We load the configuration it two steps:

   - step1: we eagerly interpret all the bits that are common to the workspace
   file and the user configuration file. The other fields are left under a lazy

   - step2: we force the interpretation of the rest of the fields

   We do that so that we can load only the general configuration part at Dune's
   initialisation time, and report errors that are more specific to OCaml later
   on *)
module Step1 = struct
  type nonrec t =
    { t : t Lazy.t
    ; config : Dune_config.t
    }
end

let step1 clflags =
  let { Clflags.x
      ; profile = cl_profile
      ; instrument_with = cl_instrument_with
      ; workspace_file = _
      ; config_from_command_line
      ; config_from_config_file
      } =
    clflags
  in
  let x = Option.map x ~f:(fun s -> Context.Target.Named s) in
  let superpose_with_command_line cl field =
    let+ x = field in
    lazy (Option.value cl ~default:(Lazy.force x))
  in
  let* () = Dune_lang.Versioned_file.no_more_lang
  and+ env = env_field_lazy
  and+ profile =
    superpose_with_command_line cl_profile
      (field "profile" (lazy_ Profile.decode) ~default:(lazy Profile.default))
  and+ instrument_with =
    superpose_with_command_line cl_instrument_with
      (field "instrument_with"
         (lazy_
            (Dune_lang.Syntax.since Stanza.syntax (2, 7)
            >>> repeat Lib_name.decode))
         ~default:(lazy []))
  and+ config_from_workspace_file =
    Dune_config.decode_fields_of_workspace_file
  in
  let+ contexts = multi_field "context" (lazy_ Context.t) in
  let config =
    create_final_config ~config_from_workspace_file ~config_from_config_file
      ~config_from_command_line
  in
  let t =
    lazy
      (let profile = Lazy.force profile in
       let instrument_with = Lazy.force instrument_with in
       let contexts =
         List.map contexts ~f:(fun f ->
             Lazy.force f ~profile_default:profile
               ~instrument_with_default:instrument_with ~x)
       in
       let env = Lazy.force env in
       let defined_names = ref Context_name.Set.empty in
       let merlin_context =
         List.fold_left contexts ~init:None ~f:(fun acc ctx ->
             let name = Context.name ctx in
             if Context_name.Set.mem !defined_names name then
               User_error.raise ~loc:(Context.loc ctx)
                 [ Pp.textf "second definition of build context %S"
                     (Context_name.to_string name)
                 ];
             defined_names :=
               Context_name.Set.union !defined_names
                 (Context_name.Set.of_list (Context.all_names ctx));
             match (ctx, acc) with
             | Opam { base = { merlin = true; _ }; _ }, Some _
             | Default { merlin = true; _ }, Some _ ->
               User_error.raise ~loc:(Context.loc ctx)
                 [ Pp.text "you can only have one context for merlin" ]
             | Opam { base = { merlin = true; _ }; _ }, None
             | Default { merlin = true; _ }, None ->
               Some name
             | _ -> acc)
       in
       let contexts =
         match contexts with
         | [] ->
           [ Context.default ~x ~profile:(Some profile)
               ~instrument_with:(Some instrument_with)
           ]
         | _ -> contexts
       in
       let merlin_context =
         match merlin_context with
         | Some _ -> merlin_context
         | None ->
           if
             List.exists contexts ~f:(function
               | Context.Default _ -> true
               | _ -> false)
           then
             Some Context_name.default
           else
             None
       in
       { merlin_context; contexts = top_sort (List.rev contexts); env; config })
  in
  { Step1.t; config }

let step1 clflags = fields (step1 clflags)

let default clflags =
  let { Clflags.x
      ; profile
      ; instrument_with
      ; workspace_file = _
      ; config_from_command_line
      ; config_from_config_file
      } =
    clflags
  in
  let x = Option.map x ~f:(fun s -> Context.Target.Named s) in
  let config =
    create_final_config ~config_from_config_file ~config_from_command_line
      ~config_from_workspace_file:Dune_config.Partial.empty
  in
  { merlin_context = Some Context_name.default
  ; contexts = [ Context.default ~x ~profile ~instrument_with ]
  ; env = Dune_env.Stanza.empty
  ; config
  }

let default_step1 clflags =
  let t = default clflags in
  { Step1.t = lazy t; config = t.config }

let load_step1 clflags p =
  Io.with_lexbuf_from_file p ~f:(fun lb ->
      if Dune_lexer.eof_reached lb then
        default_step1 clflags
      else
        parse_contents lb ~f:(fun lang ->
            String_with_vars.set_decoding_env
              (Pform.Env.initial lang.version)
              (step1 clflags)))

let filename = "dune-workspace"

let workspace_step1 =
  let open Memo.Build.O in
  let f () =
    let clflags = Clflags.t () in
    let+ workspace_file =
      match clflags.workspace_file with
      | None ->
        let p = Path.of_string filename in
        let+ exists = Fs_memo.path_exists p in
        Option.some_if exists p
      | Some p -> (
        Fs_memo.path_exists p >>| function
        | false ->
          User_error.raise
            [ Pp.textf "Workspace file %s does not exist"
                (Path.to_string_maybe_quoted p)
            ]
        | true -> Some p)
    in
    let clflags = { clflags with workspace_file } in
    match workspace_file with
    | None -> default_step1 clflags
    | Some p -> load_step1 clflags p
  in
  let memo = Memo.create "workspaces-internal" ~input:(module Unit) f in
  Memo.exec memo

let workspace_config () =
  let open Memo.Build.O in
  let+ step1 = workspace_step1 () in
  step1.config

let workspace =
  let open Memo.Build.O in
  let f () =
    let+ step1 = workspace_step1 () in
    Lazy.force step1.t
  in
  let memo = Memo.create "workspace" ~input:(module Unit) ~cutoff:equal f in
  Memo.exec memo

let update_execution_parameters t ep =
  ep
  |> Execution_parameters.set_action_stdout_on_success
       t.config.action_stdout_on_success
  |> Execution_parameters.set_action_stderr_on_success
       t.config.action_stderr_on_success

let build_contexts t = List.concat_map t.contexts ~f:Context.build_contexts
