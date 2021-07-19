open! Dune_engine
open! Stdune
module Dune_file = Dune_rules.Dune_file

(** Because the dune_init utility deals with the addition of stanzas and fields
    to dune projects and files, we need to inspect and manipulate the concrete
    syntax tree (CST) a good deal. *)
module Cst = Dune_lang.Cst

module Kind = struct
  type t =
    | Executable
    | Library
    | Project
    | Test

  let to_string = function
    | Executable -> "executable"
    | Library -> "library"
    | Project -> "project"
    | Test -> "test"

  let commands =
    [ ("executable", Executable)
    ; ("library", Library)
    ; ("project", Project)
    ; ("test", Test)
    ]
end

(** Abstractions around the kinds of files handled during initialization *)
module File = struct
  type dune =
    { path : Path.t
    ; name : string
    ; content : Cst.t list
    }

  type text =
    { path : Path.t
    ; name : string
    ; content : string
    }

  type t =
    | Dune of dune
    | Text of text

  let make_text path name content = Text { path; name; content }

  let full_path = function
    | Dune { path; name; _ }
    | Text { path; name; _ } ->
      Path.relative path name

  (** Inspection and manipulation of stanzas in a file *)
  module Stanza = struct
    let pp s =
      match Cst.to_sexp s with
      | None -> Pp.nop
      | Some s -> Dune_lang.pp s

    let libraries_conflict (a : Dune_file.Library.t) (b : Dune_file.Library.t) =
      a.name = b.name

    let executables_conflict (a : Dune_file.Executables.t)
        (b : Dune_file.Executables.t) =
      let a_names = String.Set.of_list_map ~f:snd a.names in
      let b_names = String.Set.of_list_map ~f:snd b.names in
      String.Set.inter a_names b_names |> String.Set.is_empty |> not

    let tests_conflict (a : Dune_file.Tests.t) (b : Dune_file.Tests.t) =
      executables_conflict a.exes b.exes

    let stanzas_conflict (a : Stanza.t) (b : Stanza.t) =
      let open Dune_file in
      match (a, b) with
      | Executables a, Executables b -> executables_conflict a b
      | Library a, Library b -> libraries_conflict a b
      | Tests a, Tests b -> tests_conflict a b
      (* NOTE No other stanza types currently supported *)
      | _ -> false

    let csts_conflict project (a : Cst.t) (b : Cst.t) =
      let of_ast = Dune_file.Stanzas.of_ast project in
      (let open Option.O in
      let* a_ast = Cst.abstract a in
      let+ b_ast = Cst.abstract b in
      let a_asts = of_ast a_ast in
      let b_asts = of_ast b_ast in
      List.exists
        ~f:(fun x -> List.exists ~f:(stanzas_conflict x) a_asts)
        b_asts)
      |> Option.value ~default:false

    (* TODO(shonfeder): replace with stanza merging *)
    let find_conflicting project new_stanzas existing_stanzas =
      let conflicting_stanza stanza =
        match List.find ~f:(csts_conflict project stanza) existing_stanzas with
        | Some conflict -> Some (stanza, conflict)
        | None -> None
      in
      List.find_map ~f:conflicting_stanza new_stanzas

    let add (project : Dune_project.t) stanzas = function
      | Text f -> Text f (* Adding a stanza to a text file isn't meaningful *)
      | Dune f -> (
        match find_conflicting project stanzas f.content with
        | None -> Dune { f with content = f.content @ stanzas }
        | Some (a, b) ->
          User_error.raise
            [ Pp.text "Updating existing stanzas is not yet supported."
            ; Pp.text
                "A preexisting dune stanza conflicts with a generated stanza:"
            ; Pp.nop
            ; Pp.text "Generated stanza:"
            ; pp a
            ; Pp.nop
            ; Pp.text "Pre-existing stanza:"
            ; pp b
            ])
  end

  (* Stanza *)

  let create_dir path =
    try Path.mkdir_p path with
    | Unix.Unix_error (EACCES, _, _) ->
      User_error.raise
        [ Pp.textf
            "A project directory cannot be created or accessed: Lacking \
             permissions needed to create directory %s"
            (Path.to_string_maybe_quoted path)
        ]

  let load_dune_file ~path =
    let name = "dune" in
    let full_path = Path.relative path name in
    let content =
      if not (Path.exists full_path) then
        []
      else
        match Format_dune_lang.parse_file (Some full_path) with
        | Format_dune_lang.Sexps content -> content
        | Format_dune_lang.OCaml_syntax _ ->
          User_error.raise
            [ Pp.textf "Cannot load dune file %s because it uses OCaml syntax"
                (Path.to_string_maybe_quoted full_path)
            ]
    in
    Dune { path; name; content }

  let write_dune_file (dune_file : dune) =
    let path = Path.relative dune_file.path dune_file.name in
    let version =
      Dune_lang.Syntax.greatest_supported_version Dune_engine.Stanza.syntax
    in
    Format_dune_lang.write_file ~version ~path dune_file.content

  let write f =
    let path = full_path f in
    match f with
    | Dune f -> Ok (write_dune_file f)
    | Text f ->
      if Path.exists path then
        Error path
      else
        Ok (Io.write_file ~binary:false path f.content)
end

(** The context in which the initialization is executed *)
module Init_context = struct
  type t =
    { dir : Path.t
    ; project : Dune_project.t
    }

  let make path =
    let project =
      match
        Dune_project.load ~dir:Path.Source.root ~files:String.Set.empty
          ~infer_from_opam_files:true ~dir_status:Normal
      with
      | Some p -> p
      | None -> Dune_project.anonymous ~dir:Path.Source.root
    in
    let dir =
      match path with
      | None -> Path.root
      | Some p -> Path.of_string p
    in
    File.create_dir dir;
    { dir; project }
end

module Component = struct
  module Options = struct
    module Common = struct
      type t =
        { name : Dune_lang.Atom.t
        ; libraries : Dune_lang.Atom.t list
        ; pps : Dune_lang.Atom.t list
        }
    end

    (** TODO(shonfeder): Use separate types for executables and libs (which
        would use Lib_name.t) *)
    type public_name =
      | Use_name
      | Public_name of Dune_lang.Atom.t

    let public_name_to_string = function
      | Use_name -> "<default>"
      | Public_name p -> Dune_lang.Atom.to_string p

    module Executable = struct
      type t = { public : public_name option }
    end

    module Library = struct
      type t =
        { public : public_name option
        ; inline_tests : bool
        }
    end

    module Project = struct
      module Template = struct
        type t =
          | Exec
          | Lib

        (* TODO(shonfeder) Add custom templates *)

        let of_string = function
          | "executable" -> Some Exec
          | "library" -> Some Lib
          | _ -> None

        let commands = [ ("executable", Exec); ("library", Lib) ]
      end

      module Pkg = struct
        type t =
          | Opam
          | Esy

        let commands = [ ("opam", Opam); ("esy", Esy) ]
      end

      type t =
        { template : Template.t
        ; inline_tests : bool
        ; pkg : Pkg.t
        }
    end

    module Test = struct
      type t = unit
    end

    type 'options t =
      { context : Init_context.t
      ; common : Common.t
      ; options : 'options
      }
  end

  (* Options *)

  type 'options t =
    | Executable : Options.Executable.t Options.t -> Options.Executable.t t
    | Library : Options.Library.t Options.t -> Options.Library.t t
    | Project : Options.Project.t Options.t -> Options.Project.t t
    | Test : Options.Test.t Options.t -> Options.Test.t t

  (** Internal representation of the files comprising a component *)
  type target =
    { dir : Path.t
    ; files : File.t list
    }

  (** Creates Dune language CST stanzas describing components *)
  module Stanza_cst = struct
    open Dune_lang

    module Field = struct
      let atoms : Atom.t list -> Dune_lang.t list =
        List.map ~f:(fun x -> Atom x)

      let public_name name = List [ atom "public_name"; Atom name ]

      let name name = List [ atom "name"; Atom name ]

      let inline_tests = List [ atom "inline_tests" ]

      let libraries libs = List (atom "libraries" :: atoms libs)

      let pps pps = List [ atom "preprocess"; List (atom "pps" :: atoms pps) ]

      let optional_field ~f = function
        | [] -> []
        | args -> [ f args ]

      let common (options : Options.Common.t) =
        name options.name
        :: (optional_field ~f:libraries options.libraries
           @ optional_field ~f:pps options.pps)
    end

    let make kind common_options fields =
      (* Form the AST *)
      List ((atom kind :: fields) @ Field.common common_options)
      (* Convert to a CST *)
      |> Dune_lang.Ast.add_loc ~loc:Loc.none
      |> Cst.concrete (* Package as a list CSTs *) |> List.singleton

    let add_to_list_set elem set =
      if List.mem ~equal:Dune_lang.Atom.equal set elem then
        set
      else
        elem :: set

    let public_name_field ~default = function
      | (None : Options.public_name option) -> []
      | Some Use_name -> [ Field.public_name default ]
      | Some (Public_name name) -> [ Field.public_name name ]

    let executable (common : Options.Common.t) (options : Options.Executable.t)
        =
      let public_name = public_name_field ~default:common.name options.public in
      make "executable" common public_name

    let library (common : Options.Common.t) (options : Options.Library.t) =
      let common, inline_tests =
        if not options.inline_tests then
          (common, [])
        else
          let pps =
            add_to_list_set
              (Dune_lang.Atom.of_string "ppx_inline_test")
              common.pps
          in
          ({ common with pps }, [ Field.inline_tests ])
      in
      let public_name = public_name_field ~default:common.name options.public in
      make "library" common (public_name @ inline_tests)

    let test common (() : Options.Test.t) = make "test" common []
  end

  (* TODO Support for merging in changes to an existing stanza *)
  let add_stanza_to_dune_file ~(project : Dune_project.t) ~dir stanza =
    File.load_dune_file ~path:dir |> File.Stanza.add project stanza

  module Make = struct
    let bin ({ context; common; options } : Options.Executable.t Options.t) =
      let dir = context.dir in
      let bin_dune =
        Stanza_cst.executable common options
        |> add_stanza_to_dune_file ~project:context.project ~dir
      in
      let bin_ml =
        let name = sprintf "%s.ml" (Dune_lang.Atom.to_string common.name) in
        let content = sprintf "let () = print_endline \"Hello, World!\"\n" in
        File.make_text dir name content
      in
      let files = [ bin_dune; bin_ml ] in
      [ { dir; files } ]

    let src ({ context; common; options } : Options.Library.t Options.t) =
      let dir = context.dir in
      let lib_dune =
        Stanza_cst.library common options
        |> add_stanza_to_dune_file ~project:context.project ~dir
      in
      let files = [ lib_dune ] in
      [ { dir; files } ]

    let test ({ context; common; options } : Options.Test.t Options.t) =
      (* Marking the current absence of test-specific options *)
      let dir = context.dir in
      let test_dune =
        Stanza_cst.test common options
        |> add_stanza_to_dune_file ~project:context.project ~dir
      in
      let test_ml =
        let name = sprintf "%s.ml" (Dune_lang.Atom.to_string common.name) in
        let content = "" in
        File.make_text dir name content
      in
      let files = [ test_dune; test_ml ] in
      [ { dir; files } ]

    let proj_exec dir
        ({ context; common; options } : Options.Project.t Options.t) =
      let lib_target =
        src
          { context = { context with dir = Path.relative dir "lib" }
          ; options = { public = None; inline_tests = options.inline_tests }
          ; common
          }
      in
      let test_target =
        test
          { context = { context with dir = Path.relative dir "test" }
          ; options = ()
          ; common
          }
      in
      let bin_target =
        (* Add the lib_target as a library to the executable*)
        let libraries =
          Stanza_cst.add_to_list_set common.name common.libraries
        in
        bin
          { context = { context with dir = Path.relative dir "bin" }
          ; options = { public = Some (Options.Public_name common.name) }
          ; common =
              { common with libraries; name = Dune_lang.Atom.of_string "main" }
          }
      in
      bin_target @ lib_target @ test_target

    let proj_lib dir
        ({ context; common; options } : Options.Project.t Options.t) =
      let lib_target =
        src
          { context = { context with dir = Path.relative dir "lib" }
          ; options =
              { public = Some (Options.Public_name common.name)
              ; inline_tests = options.inline_tests
              }
          ; common
          }
      in
      let test_target =
        test
          { context = { context with dir = Path.relative dir "test" }
          ; options = ()
          ; common
          }
      in
      lib_target @ test_target

    let proj
        ({ context; common; options } as opts : Options.Project.t Options.t) =
      let ({ template; pkg; _ } : Options.Project.t) = options in
      let dir =
        Path.relative context.dir (Dune_lang.Atom.to_string common.name)
      in
      let name =
        Package.Name.parse_string_exn
          (Loc.none, Dune_lang.Atom.to_string common.name)
      in
      let proj_target =
        let files =
          match (pkg : Options.Project.Pkg.t) with
          | Opam ->
            let opam_file = Package.file ~dir ~name in
            [ File.make_text
                (Path.parent_exn opam_file)
                (Path.basename opam_file) ""
            ]
          | Esy -> [ File.make_text dir "package.json" "" ]
        in
        { dir; files }
      in
      let component_targets =
        match (template : Options.Project.Template.t) with
        | Exec -> proj_exec dir opts
        | Lib -> proj_lib dir opts
      in
      proj_target :: component_targets
  end

  let report_uncreated_file = function
    | Ok _ -> ()
    | Error path ->
      let open Pp.O in
      User_warning.emit
        [ Pp.textf "File "
          ++ Pp.tag User_message.Style.Kwd
               (Pp.verbatim (Path.to_string_maybe_quoted path))
          ++ Pp.text " was not created because it already exists"
        ]

  (** Creates a component, writing the files to disk *)
  let create target =
    File.create_dir target.dir;
    List.map ~f:File.write target.files

  let init (type options) (t : options t) =
    let target =
      match t with
      | Executable params -> Make.bin params
      | Library params -> Make.src params
      | Project params -> Make.proj params
      | Test params -> Make.test params
    in
    List.concat_map ~f:create target |> List.iter ~f:report_uncreated_file
end
