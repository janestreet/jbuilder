open! Dune_engine
open! Stdune

(* This module expands either a (library ... (ctypes ...)) rule or an
   (executables ... (ctypes ...)) rule into the generated set of .ml and .c
   files needed to conveniently write OCaml bindings for C libraries.

   Aside from perhaps providing an "header.h" include line, you should be
   able to wrap an entire C library without writing a single line of C code.

   This stanza requires the user to specify the names of 4 (or more) modules:

    (type_description Type_description)
    (generated_types Types_generated)
    (function_description Function_description) ; can be repeated
    (generated_entry_point C)

   The user must also implement two of the modules:

   (1) $type_description.ml must have the following top-level functor:

    module Types (T : Ctypes.TYPE) = struct
      (* put calls to Ctypes.TYPE.constant and Ctypes.TYPE.typedef here
         to wrap C constants and structs *)
    end

   (2) $function_description.ml must have the following two definitions:

    modules Types = $generated_types

    module Functions (F : Ctypes.FOREIGN) = struct
      (* put calls to F.foreign here to wrap C functions *)
    end

   Once the above modules are provided, the ctypes stanza will:
   - generate a types generator
   - generate a functions generator
   - set up a discovery program to query pkg-config for compile and link flags,
     if you decided to use pkg-config instead of vendored c-flags
   - use the types/data and functions modules you filled in with a functor
     to tie everything into your library.

   The user must also specify the name of a final "Entry point" output module
   ($generated_entry_point) that will be generated and added to your executable
   or library.  Suggest calling it "C" and accessing the instantiated functors
   from your project as C.Types and C.Functions.

   It may help to view a real world example of all of the boilerplate that is
   being replaced by a [ctypes] stanza:

   https://github.com/mbacarella/mpg123/blob/077a72d922931eb46d4b4e5842b0426fa3c161b5/c/dune

   This implementation is not, however, a naive translation of the boilerplate
   above.  This module uses dune internal features to simplify the stub
   generation.  As a result, there are no intermediate libraries (or packages).
*)

module Buildable = Dune_file.Buildable
module Library =  Dune_file.Library
module Ctypes = Ctypes_stanza

module Stanza_util = struct

  let sprintf = Printf.sprintf

  let discover_script ctypes =
    sprintf "%s__ctypes_discover" ctypes.Ctypes.external_library_name

  let type_gen_script ctypes =
    sprintf "%s__type_gen" ctypes.Ctypes.external_library_name

  let module_name_lower_string module_name =
    String.lowercase (Module_name.to_string module_name)

  let function_gen_script ctypes fd =
    sprintf "%s__function_gen__%s__%s" ctypes.Ctypes.external_library_name
      (module_name_lower_string fd.Ctypes.Function_description.functor_)
      (module_name_lower_string fd.Ctypes.Function_description.instance)

  let type_description_functor ctypes =
    ctypes.Ctypes.type_description.functor_

  let type_description_instance ctypes =
    ctypes.Ctypes.type_description.instance

  let entry_module ctypes =
    ctypes.Ctypes.generated_entry_point

  let cflags_sexp ctypes =
    Ctypes_stubs.cflags_sexp
      ~external_library_name:ctypes.Ctypes.external_library_name

  let c_library_flags_sexp ctypes =
    sprintf "%s__c_library_flags.sexp" ctypes.Ctypes.external_library_name

  let c_generated_types_module ctypes =
    sprintf "%s__c_generated_types" ctypes.Ctypes.external_library_name
    |> Module_name.of_string

  let c_generated_functions_module ctypes fd =
    sprintf "%s__c_generated_functions__%s__%s"
      ctypes.Ctypes.external_library_name
      (module_name_lower_string fd.Ctypes.Function_description.functor_)
      (module_name_lower_string fd.Ctypes.Function_description.instance)
    |> Module_name.of_string

  (* This includer module is simply some glue to instantiate the Types functor
     that the user provides in the type description module. *)
  let c_types_includer_module ctypes =
    ctypes.Ctypes.generated_types

  let c_generated_functions_cout_c ctypes fd =
    sprintf "%s__c_cout_generated_functions__%s__%s.c"
      ctypes.Ctypes.external_library_name
      (module_name_lower_string fd.Ctypes.Function_description.functor_)
      (module_name_lower_string fd.Ctypes.Function_description.instance)

  let lib_deps_of_strings ~loc lst =
    List.map lst ~f:(fun lib ->
      Lib_dep.Direct (loc, Lib_name.of_string lib))

  let type_gen_script_module ctypes =
    type_gen_script ctypes |> Module_name.of_string

  let function_gen_script_module ctypes function_description =
    function_gen_script ctypes function_description |> Module_name.of_string

  let generated_modules ctypes =
    List.concat_map ctypes.Ctypes.function_description ~f:(fun function_description ->
      [ function_gen_script_module ctypes function_description
      ; c_generated_functions_module ctypes function_description ])
    @
    [ type_gen_script_module ctypes
    ; c_generated_types_module ctypes
    ; c_types_includer_module ctypes
    ; entry_module ctypes ]

  let non_installable_modules ctypes =
    type_gen_script_module ctypes
    :: List.map ctypes.Ctypes.function_description ~f:(fun function_description ->
      function_gen_script_module ctypes function_description)

  let generated_ml_and_c_files ctypes =
    let ml_files =
      generated_modules ctypes
      |> List.map ~f:Module_name.to_string
      |> List.map ~f:String.lowercase
      |> List.map ~f:(fun m -> m ^ ".ml")
    in
    let c_files =
      List.map ctypes.Ctypes.function_description ~f:(fun fd ->
        c_generated_functions_cout_c ctypes fd)
    in
    ml_files @ c_files
end

let generated_ml_and_c_files = Stanza_util.generated_ml_and_c_files
let non_installable_modules = Stanza_util.non_installable_modules

let ml_of_module_name mn =
  Module_name.to_string mn ^ ".ml"
  |> String.lowercase

let modules_of_list ~dir ~modules =
  let name_map =
    let build_dir = Path.build dir in
    let modules =
      List.map modules ~f:(fun name ->
        let module_name = Module_name.of_string name in
        let path = Path.relative build_dir (String.lowercase name ^ ".ml") in
        let impl = Module.File.make Dialect.ocaml path in
        let source = Module.Source.make ~impl module_name in
        Module.of_source ~visibility:Visibility.Public
          ~kind:Module.Kind.Impl source)
    in
    Module.Name_map.of_list_exn modules
  in
  Modules.exe_unwrapped name_map
  (* Modules.exe_wrapped ~src_dir:dir ~modules:name_map *)

let write_c_types_includer_module ~sctx ~dir ~filename
      ~type_description_functor ~c_generated_types_module =
  let path = Path.Build.relative dir filename in
  let contents =
    let buf = Buffer.create 1024 in
    let pr buf fmt = Printf.bprintf buf (fmt ^^ "\n") in
    pr buf "include %s.Types (%s)"
        (Module_name.to_string type_description_functor)
        (Module_name.to_string c_generated_types_module);
    Buffer.contents buf
  in
  Super_context.add_rule ~loc:Loc.none sctx ~dir
    (Action_builder.write_file path contents)

let write_entry_point_module ~ctypes ~sctx ~dir ~filename
      ~type_description_instance ~function_description
      ~c_types_includer_module =
  let path = Path.Build.relative dir filename in
  let contents =
    let buf = Buffer.create 1024 in
    let pr buf fmt = Printf.bprintf buf (fmt ^^ "\n") in
    pr buf "module %s = %s"
      (Module_name.to_string type_description_instance)
      (Module_name.to_string c_types_includer_module);
    List.iter function_description ~f:(fun fd ->
      let c_generated_functions_module =
        Stanza_util.c_generated_functions_module ctypes fd
      in
      pr buf "module %s = %s.Functions (%s)"
        (fd.Ctypes.Function_description.instance |> Module_name.to_string)
        (fd.Ctypes.Function_description.functor_ |> Module_name.to_string)
        (Module_name.to_string c_generated_functions_module)
    );
    Buffer.contents buf
  in
  Super_context.add_rule ~loc:Loc.none sctx ~dir
    (Action_builder.write_file path contents)

let discover_gen ~external_library_name:lib ~cflags_sexp ~c_library_flags_sexp =
  let buf = Buffer.create 1024 in
  let pr buf fmt = Printf.bprintf buf (fmt ^^ "\n") in
  pr buf "module C = Configurator.V1";
  pr buf "let () =";
  pr buf "  C.main ~name:\"%s\" (fun c ->" lib;
  pr buf "    let default : C.Pkg_config.package_conf =";
  pr buf "      { libs   = [\"-l%s\"];" lib;
  pr buf "        cflags = [\"-I/usr/include\"] }";
  pr buf "    in";
  pr buf "    let conf =";
  pr buf "      match C.Pkg_config.get c with";
  pr buf "      | None -> default";
  pr buf "      | Some pc ->";
  pr buf "        match C.Pkg_config.query pc ~package:\"%s\" with" lib;
  pr buf "        | None -> default";
  pr buf "        | Some deps -> deps";
  pr buf "    in";
  pr buf "    C.Flags.write_sexp \"%s\" conf.cflags;" cflags_sexp;
  pr buf "    C.Flags.write_sexp \"%s\" conf.libs;" c_library_flags_sexp;
  pr buf "  )";
  Buffer.contents buf

let write_discover_script ~filename ~sctx ~dir ~external_library_name
      ~cflags_sexp ~c_library_flags_sexp =
  let path = Path.Build.relative dir filename in
  let script =
    discover_gen ~external_library_name ~cflags_sexp ~c_library_flags_sexp
  in
  Super_context.add_rule ~loc:Loc.none sctx ~dir
    (Action_builder.write_file path script)

let gen_headers headers buf =
  let pr buf fmt = Printf.bprintf buf (fmt ^^ "\n") in
  begin match headers with
  | Ctypes.Headers.Include lst ->
    List.iter lst ~f:(fun h -> pr buf "  print_endline \"#include <%s>\";" h)
  | Preamble s ->
    pr buf "  print_endline %S;" s
  end

let type_gen_gen ~headers ~type_description_functor =
  let buf = Buffer.create 1024 in
  let pr buf fmt = Printf.bprintf buf (fmt ^^ "\n") in
  pr buf "let () =";
  gen_headers headers buf;
  pr buf "  Cstubs_structs.write_c Format.std_formatter";
  pr buf "    (module %s.Types)" (Module_name.to_string type_description_functor);
  Buffer.contents buf

let function_gen_gen ~concurrency ~headers ~function_description_functor =
  let module_name = Module_name.to_string function_description_functor in
  let concurrency =
    match concurrency with
    | Ctypes.Concurrency_policy.Unlocked -> "Cstubs.unlocked"
    | Sequential -> "Cstubs.sequential"
    | Lwt_jobs -> "Cstubs.lwt_jobs"
    | Lwt_preemptive -> "Cstubs.lwt_preemptive"
  in
  let buf = Buffer.create 1024 in
  let pr buf fmt = Printf.bprintf buf (fmt ^^ "\n") in
  pr buf "let () =";
  pr buf "  let concurrency = %s in" concurrency;
  pr buf "  let prefix = Sys.argv.(2) in";
  pr buf "  match Sys.argv.(1) with";
  pr buf "  | \"ml\" ->";
  pr buf "    Cstubs.write_ml ~concurrency Format.std_formatter ~prefix";
  pr buf "      (module %s.Functions)" module_name;
  pr buf "  | \"c\" ->";
  gen_headers headers buf;
  pr buf "    Cstubs.write_c ~concurrency Format.std_formatter ~prefix";
  pr buf "      (module %s.Functions)" module_name;
  pr buf "  | s -> failwith (\"unknown functions \"^s)";
  Buffer.contents buf

let write_type_gen_script ~headers ~dir ~filename ~sctx
      ~type_description_functor =
  let path = Path.Build.relative dir filename in
  let script = type_gen_gen ~headers ~type_description_functor in
  Super_context.add_rule ~loc:Loc.none sctx ~dir
    (Action_builder.write_file path script)

let write_function_gen_script ~headers ~sctx ~dir ~name
      ~function_description_functor ~concurrency =
  let path = Path.Build.relative dir (name ^ ".ml") in
  let script = function_gen_gen ~concurrency ~headers ~function_description_functor in
  Super_context.add_rule ~loc:Loc.none sctx ~dir (Action_builder.write_file path script)

let rule ?(deps=[]) ?stdout_to ?(args=[]) ?(targets=[]) ~exe ~sctx ~dir () =
  let build =
    let exe = Ok (Path.build (Path.Build.relative dir exe)) in
    let args =
      let targets = List.map targets ~f:(Path.Build.relative dir) in
      let deps =
        List.map deps ~f:(Path.relative (Path.build dir))
        |> Dep.Set.of_files
      in
      let open Command.Args in
      [ Hidden_targets targets
      ; Hidden_deps deps
      ; As args ]
    in
    let stdout_to = Option.map stdout_to ~f:(Path.Build.relative dir) in
    Command.run exe ~dir:(Path.build dir) ?stdout_to args
  in
  Super_context.add_rule sctx ~dir build

let build_c_program ~sctx ~dir ~source_files ~scope ~cflags_sexp ~output () =
  let ctx = Super_context.context sctx in
  let open Memo.Build.O in
  let* exe =
    Ocaml_config.c_compiler ctx.Context.ocaml_config
    |> Super_context.resolve_program ~loc:None ~dir sctx
  in
  let include_args =
    (* XXX: need glob dependency *)
    let ocaml_where = Path.to_string ctx.Context.stdlib_dir in
    (* XXX: need glob dependency *)
    let open Resolve.O in
    let+ ctypes_include_dirs =
      let+ lib =
        let ctypes = Lib_name.of_string "ctypes" in
        Lib.DB.resolve (Scope.libs scope) (Loc.none, ctypes)
        (*
        | Ok lib -> lib
        | Error _res ->
          User_error.raise
            [ Pp.textf "the 'ctypes' library needs to be installed to use the ctypes stanza"]
           *)
      in
      Lib.L.include_paths [lib] Mode.Native
      |> Path.Set.to_list
      |> List.map ~f:Path.to_string
    in
    let include_dirs = ocaml_where :: ctypes_include_dirs in
    List.concat_map include_dirs ~f:(fun dir -> ["-I"; dir])
  in
  let deps =
    List.map source_files ~f:(Path.relative (Path.build dir))
    |> Dep.Set.of_files
  in
  let build =
    let cflags_args =
      let contents = Action_builder.contents (Path.relative (Path.build dir) cflags_sexp) in
      Action_builder.map contents ~f:(fun sexp ->
        let fail s = User_error.raise [ Pp.textf s ] in
        let ast =
          Dune_lang.Parser.parse_string ~mode:Dune_lang.Parser.Mode.Single
            ~fname:cflags_sexp sexp
        in
        match ast with
        | Dune_lang.Ast.Atom (_loc, atom) -> [Dune_lang.Atom.to_string atom]
        | Template _ -> fail "'template' not supported in ctypes c_flags"
        | Quoted_string (_loc, s) -> [s]
        | List (_loc, lst) ->
          List.map lst ~f:(function
            | Dune_lang.Ast.Atom (_loc, atom) -> Dune_lang.Atom.to_string atom
            | Quoted_string (_loc, s) -> s
            | Template _ -> fail "'template' not supported in ctypes c_flags"
            | List _ -> fail "nested lists not supported in ctypes c_flags"))
    in
    let absolute_path_hack p =
      (* These normal path builder things construct relative paths like
         _build/default/your/project/file.c but before dune runs gcc it
         actually cds into _build/default, which fails, so we turn them
         into absolutes to hack around it. *)
      Path.relative (Path.build dir) p |> Path.to_absolute_filename
    in
    let action =
      let open Action_builder.O in
      let* include_args = Resolve.read include_args in
      Action_builder.deps deps
      >>> Action_builder.map cflags_args ~f:(fun cflags_args ->
        let source_files = List.map source_files ~f:absolute_path_hack in
        let output = absolute_path_hack output in
        let args = cflags_args @ include_args @ source_files @ ["-o"; output] in
        Action.run exe args)
    in
    Action_builder.with_targets action ~targets:[Path.Build.relative dir output]
  in
  Super_context.add_rule sctx ~dir build

let cctx_with_substitutions ?(libraries=[]) ~modules ~dir
      ~loc ~scope ~cctx =
  let compile_info =
    let dune_version = Scope.project scope |> Dune_project.dune_version in
    Lib.DB.resolve_user_written_deps_for_exes (Scope.libs scope)
      [ (loc, "ctypes") ]
      (Stanza_util.lib_deps_of_strings ~loc libraries)
      ~dune_version ~pps:[]
  in
  let modules = modules_of_list ~dir ~modules in
  let module Cctx = Compilation_context in
  Cctx.create
    ~super_context:(Cctx.super_context cctx)
    ~scope:(Cctx.scope cctx)
    ~expander:(Cctx.expander cctx)
    ~js_of_ocaml:(Cctx.js_of_ocaml cctx)
    ~package:(Cctx.package cctx)
    ~flags:(Cctx.flags cctx)
    ~requires_compile:(Lib.Compile.direct_requires compile_info)
    ~requires_link:(Lib.Compile.requires_link compile_info)
    ~obj_dir:(Cctx.obj_dir cctx)
    ~opaque:(Cctx.Explicit (Cctx.opaque cctx))
    ~modules ()

let program_of_module_and_dir ~dir program =
  let build_dir = Path.build dir in
  Exe.Program.{
    name = program;
    main_module_name = Module_name.of_string program;
    loc = Loc.in_file (Path.relative build_dir program)
  }

let exe_build_and_link ?libraries ?(modules=[]) ~scope ~loc ~dir ~cctx program =
  let cctx =
    cctx_with_substitutions ?libraries ~loc ~scope ~dir ~cctx
      ~modules:(program :: modules)
  in
  let program = program_of_module_and_dir ~dir program in
  Exe.build_and_link ~program ~linkages:[Exe.Linkage.native]
    ~promote:None cctx

let exe_link_only ~dir ~shared_cctx ~dep_graphs program =
  let program = program_of_module_and_dir ~dir program in
  Exe.link_many ~programs:[program] ~linkages:[Exe.Linkage.native]
    ~dep_graphs ~promote:None shared_cctx

let write_osl_to_sexp_file ~sctx ~dir ~filename osl =
  let build =
    let path = Path.Build.relative dir filename in
    let sexp =
      let encoded =
        match Ordered_set_lang.Unexpanded.encode osl with
        | [s] -> s
        | _lst ->
          User_error.raise
            [ Pp.textf "expected %s to contain a list of atoms" filename]
      in
      Dune_lang.to_string encoded
    in
    Action_builder.write_file path sexp
  in
  Super_context.add_rule ~loc:Loc.none sctx ~dir build

(* Adding an 'iter' to Memo.Build produced pretty strange far-flung type
   errors, so just doing this here. *)
let rec memo_build_list_iter lst ~f =
  let open Memo.Build.O in
  match lst with
  | [] -> Memo.Build.return ()
  | x :: tl ->
    let* () = f x in
    memo_build_list_iter tl ~f

let gen_rules ~dep_graphs ~cctx ~buildable ~loc ~scope ~dir ~sctx =
  let ctypes = Option.value_exn buildable.Buildable.ctypes in
  let external_library_name = ctypes.Ctypes.external_library_name in
  let type_description_functor = Stanza_util.type_description_functor ctypes in
  let c_types_includer_module = Stanza_util.c_types_includer_module ctypes in
  let c_generated_types_module = Stanza_util.c_generated_types_module ctypes in
  let rule = rule ~sctx ~dir in
  let open Memo.Build.O in
  let* () =
    write_c_types_includer_module
      ~sctx ~dir ~filename:(ml_of_module_name c_types_includer_module)
      ~c_generated_types_module ~type_description_functor
  in
  (* The output of this process is to generate a cflags sexp and a c library
     flags sexp file.  We can probe these flags by using the system pkg-config,
     if it's an external system library.  The user could also tell us what
     they are, if the library is vendored.

     https://dune.readthedocs.io/en/stable/quick-start.html#defining-a-library-with-c-stubs-using-pkg-config
  *)
  let c_library_flags_sexp = Stanza_util.c_library_flags_sexp ctypes in
  let cflags_sexp = Stanza_util.cflags_sexp ctypes in
  let* () =
    let open Ctypes.Build_flags_resolver in
    match ctypes.Ctypes.build_flags_resolver with
    | Vendored { c_flags; c_library_flags } ->
      let* () =
        write_osl_to_sexp_file ~sctx ~dir ~filename:cflags_sexp c_flags
      in
      write_osl_to_sexp_file ~sctx ~dir ~filename:c_library_flags_sexp
        c_library_flags
    | Pkg_config ->
      let cflags_sexp = Stanza_util.cflags_sexp ctypes in
      let discover_script = Stanza_util.discover_script ctypes in
      let* () =
        write_discover_script
          ~sctx ~dir ~filename:(discover_script ^ ".ml") ~cflags_sexp
          ~c_library_flags_sexp ~external_library_name
      in
      let* () =
        exe_build_and_link ~scope ~loc ~dir ~cctx
          ~libraries:["dune.configurator"] discover_script
      in
      rule
        ~targets:[cflags_sexp; c_library_flags_sexp]
        ~exe:(discover_script ^ ".exe") ()
  in
  let generated_entry_module = Stanza_util.entry_module ctypes in
  let headers = ctypes.Ctypes.headers in
  let exe_link_only = exe_link_only ~dir ~shared_cctx:cctx ~dep_graphs in
  (* Type_gen produces a .c file, taking your type description module above
     as an input.
     The .c file is compiled into an .exe.
     The .exe, when run produces an .ml file.
     The .ml file is compiled into a module that will have the user's
     ML-wrapped C data/types.

     Note the similar function_gen process below depends on the ML-wrapped C
     data/types produced in this step. *)
  let* () =
    let c_generated_types_cout_c =
      sprintf "%s__c_cout_generated_types.c" external_library_name
    in
    let c_generated_types_cout_exe =
      sprintf "%s__c_cout_generated_types.exe" external_library_name
    in
    let type_gen_script = Stanza_util.type_gen_script ctypes in
    let* () =
      write_type_gen_script ~headers ~sctx ~dir
        ~filename:(type_gen_script ^ ".ml")
        ~type_description_functor
    in
    let* () = exe_link_only type_gen_script in
    let* () =
      rule
        ~stdout_to:c_generated_types_cout_c
        ~exe:(type_gen_script ^ ".exe") ()
    in
    let* () =
      build_c_program
        ~sctx ~dir ~scope ~cflags_sexp
        ~source_files:[c_generated_types_cout_c]
        ~output:c_generated_types_cout_exe ()
    in
    rule
      ~stdout_to:(c_generated_types_module |> ml_of_module_name)
      ~exe:(c_generated_types_cout_exe) ()
  in
  (* Function_gen is similar to type_gen above, though it produces both an
     .ml file and a .c file.  These files correspond to the files you would
     have to write by hand to wrap C code (if ctypes didn't exist!)

     Also the user can repeat the 'function_description' stanza to do this
     more than once.  This is needed for generating blocking and non-blocking
     sets of functions, for example, which requires a different 'concurrency'
     parameter in the code generator. *)
  let* () =
    memo_build_list_iter ctypes.Ctypes.function_description ~f:(fun fd ->
      let stubs_prefix = external_library_name ^ "_stubs" in
      let c_generated_functions_cout_c =
        Stanza_util.c_generated_functions_cout_c ctypes fd
      in
      let function_gen_script = Stanza_util.function_gen_script ctypes fd in
      let* () =
        write_function_gen_script ~headers ~sctx ~dir
          ~name:function_gen_script
          ~concurrency:fd.Ctypes.Function_description.concurrency
          ~function_description_functor:fd.Ctypes.Function_description.functor_
      in
      let* () = exe_link_only function_gen_script in
      let* () =
        rule
          ~stdout_to:c_generated_functions_cout_c
          ~exe:(function_gen_script ^ ".exe")
          ~args:["c"; stubs_prefix] ()
      in
      rule
        ~stdout_to:(Stanza_util.c_generated_functions_module ctypes fd
                    |> ml_of_module_name)
        ~exe:(function_gen_script ^ ".exe")
        ~args:["ml"; stubs_prefix]
        ()
    )
  in
  (* The entry point module binds the instantiated Types and Functions functors
     to the entry point module name and instances the user specified. *)
  write_entry_point_module ~ctypes ~sctx ~dir
    ~filename:(generated_entry_module |> ml_of_module_name)
    ~type_description_instance:(Stanza_util.type_description_instance ctypes)
    ~function_description:ctypes.Ctypes.function_description
    ~c_types_includer_module

let ctypes_cclib_flags ~standard ~scope ~expander ~buildable =
  match buildable.Buildable.ctypes with
  | None -> standard
  | Some ctypes ->
    let ctypes_c_library_flags =
      let path_to_sexp_file =
        Ctypes_stubs.c_library_flags
          ~external_library_name:ctypes.Ctypes.external_library_name
      in
      let parsing_context =
        let project = Scope.project scope in
        Dune_project.parsing_context project
      in
      Ordered_set_lang.Unexpanded.include_single
        ~context:parsing_context ~pos:("", 0, 0, 0)
        path_to_sexp_file
    in
    Expander.expand_and_eval_set expander ctypes_c_library_flags
      ~standard
