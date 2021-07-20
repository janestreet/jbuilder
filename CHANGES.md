Unreleased
----------

- Fixes `dune exec` not adding .exe on Windows (#4371, fixes #3322, @MisterDA)

- Allow multiple cinaps stanzas in the same directory (#4460, @rgrinberg)

- Fix `$ dune subst` in empty git repositories (#4441, fixes #3619, @rgrinberg)

- Improve interpretation of ansi escape sequence when spawning processes (#4408,
  fixes #2665, @rgrinberg)

- Allow `(package pkg)` in dependencies even if `pkg` is an installed package
  (#4170, @bobot)

- Allow `%{version:pkg}` to work for external packages (#4104, @kit-ty-kate)

- Add `(glob_files_rec <dir>/<glob>)` for globbing files recursively (#4176,
  @jeremiedimino)

- Automatically generate empty `.mli` files for executables and tests (#3768,
  fixes #3745, @CraigFe)

- Add `ocaml` command subgroup for OCaml related commands such as `utop`, `top`,
  and `merlin` (#3936, @rgrinberg).

- Detect unknown variables more eagerly (#4184, @jeremiedimino)

- Improve location of variables and macros in error messages (#4205,
  @jeremiedimino)

- Auto-detect `dune-project` files as `dune` files in Emacs (#4222, @shonfeder)

- Dune no longer automatically create or edit `dune-project` files
  (#4239, fixes #4108, @jeremiedimino)

- Cleanup temporary files after running `$ dune exec`. (#4260, fixes #4243,
  @rgrinberg)

- Add a new subcommand `dune ocaml dump-dot-merlin` that prints a mix of all the
  merlin configuration of a directory (defaulting to the current directory) in
  the Merlin configuration syntax. (#4250, @voodoos)

- Enable cram tests by default (#4262, @rgrinberg)

- Drop support for opam 1.x (#4280, @jeremiedimino)

- Stop calling `ocamlfind` to determine the library search path or
  library installation directory. This makes the behavior of Dune
  simpler and more reproducible (#4281, @jeremiedimino)

- Remove the `external-lib-deps` command. This command was only
  approximative and the cost of maintainance was getting too high. We
  removed it to make room for new more important features (#4298,
  @jeremiedimino)

- It is now possible to define action dependencies through a chain
  of aliases. (#4303, @aalekseyev)

- If an .ml file is not used by an executable, Dune no longer report
  parsing error in this file (#4330, @jeremiedimino)

- Add support for sandboxing using hard links (#4360, @snowleopard)

- Fix dune crash when `subdir` is an absolute path (#4366, @anmonteiro)

- Changed the implementation of actions attached to aliases, as in
  `(rule (alias runtest) (action (run ./test)))`. A visible result for
  users is that such actions are now memoized for longer. For
  instance:
  ```
  $ echo '(rule (alias runtest) (action (echo "X=%{env:X=0}\n")))` > dune
  $ X=1 dune runtest
  X=1
  $ X=2 dune runtest
  X=2
  $ X=1 dune runtest
  ```
  Previously, Dune would have re-executed the action again at the last
  line. Now it remembers the result of the first execution.

- Fix a bug where dune would always re-run all actions that produce symlinks,
  even if their dependencies did not change. (#4405, @aalekseyev)

- Fix a bug that was causing Dune to re-hash generated files more
  often than necessary (#4419, @jeremiedimino)

- Fields allowed in the config file are now also allowed in the
  workspace file (#4426, @jeremiedimino)

- Add options to control how Dune should handle stdout and stderr of
  actions when then succeed. It is now possible to ask Dune to ignore
  the stdout of actions when they succeed or to request that the
  stderr of actions must be empty. This allows to reduce the noise of
  large builds (#4422, #4515, @jeremiedimino)

- The `@all` alias no longer depends directly on copies of files from the source
  directory (#4461, @nojb)

- Allow dune-file as an alternative file name for dune files (needs to be
  enabled in the dune-project file) (#4428, @nojb)

- Drop support for upgrading jbuilder projects (#4473, @jeremiedimino)

- Extend the environment variable `BUILD_PATH_PREFIX_MAP` to rewrite
  the root of the build dir (or sandbox) to `/workspace_root` (#4466,
  @jeremiedimino)

- Simplify the implementation of build cache. We stop using the cache daemon to
  access the cache and instead write to and read from it directly. The new cache
  implementation is based on Jenga's cache library, which was thoroughly tested
  on large-scale builds. Using Jenga's cache library will also make it easier
  for us to port Jenga's cloud cache to Dune. (#4443, #4465, Andrey Mokhov)

- More informative error message when Dune can't read a target that's supposed
  to be produced by the action. Old message is still produced on ENOENT, but other
  errors deserve a more detailed report. (#4501, @aalekseyev)

- Fixed a bug where a sandboxed action would fail if it declares no dependencies in
  its initial working directory or any directory it `chdir`s into. (#4509, @aalekseyev)

- Fix a crash when clearing temporary directories (#4489, #4529, Andrey Mokhov)

- Dune now memoizes all errors when running in the file-watching mode. This
  speeds up incremental rebuilds but may be inconvenient in rare cases, e.g. if
  a build action fails due to a spurious error, such as running out of memory.
  Right now, the only way to force such actions to be rebuilt is to restart
  Dune, which clears all memoized errors. In future, we would like to provide a
  way to rerun all actions failed due to errors without restarting the build,
  e.g. via a Dune RPC call. (#4522, Andrey Mokhov)

- Remove `dune compute`. It was broken and unused (#4540,
  @jeremiedimino)

- No longer generate an approximate merlin files when computing the
  ocaml flags fails, for instance because they include the contents of
  a file that failed to build. This was a niche feature and it was
  getting in the way of making Dune's core better. (#4607, @jeremiedimino)

- Make Dune display the progress indicator in all output modes except quiet
  (#4618, @aalekseyev)

- Do not log `live_words` and `free_words` in trace file. This allows using
  `Gc.quick_stat` which does not scan the heap. (#4643, @emillon)

- Don't let command run by Dune observe the environment variable
  `INSIDE_EMACS` in order to improve reproducibility (#4680,
  @jeremiedimino)

- Fix `root_module` when used in public libraries (#4685, fixes #4684,
  @rgrinberg, @CraigFe)

- Fix `root_module` when used with preprocessing (#4683, fixes #4682,
  @rgrinberg, @CraigFe)

- Display Coq profile flags in `dune printenv` (#4767, @ejgallego)

- Introduce mdx stanza 0.2, requiring mdx >= 1.9.0, with a new generic `deps`
  field and the possibility to statically link `libraries` in the test
  executable. (#3956, fixes #3955)

- Allow depending on `ocamldoc` library when `ocamlfind` is not installed.
  (#4811, fixes #4809, @nojb)

2.9.1 (unreleased)
------------------

- Don't use `subst --root` in Opam files (#4806, @MisterDA)

2.9.0 (29/06/2021)
------------------

- Add `(enabled_if ...)` to `(mdx ...)` (#4434, @emillon)

- Add support for instrumentation dependencies (#4210, fixes #3983, @nojb)

- Add the possibility to use `locks` with the cram tests stanza (#4397, @voodoos)

- Allow to set up merlin in a variant of the default context
  (#4145, @TheLortex, @voodoos)

- Add `(package ...)` to `(mdx ...)` (#4691, fixes #3756, @emillon)

- Handle renaming of `coq.kernel` library to `coq-core.kernel` in Coq 8.14 (#4713, @proux01)

- Fix generation of merlin configuration when using `(include_subdirs
  unqualified)` on Windows (#4745, @nojb)

- Fix bug for the install of Coq native files when using `(include_subdirs qualified)`
  (#4753, @ejgallego)

- Allow users to specify install target directories for `doc` and
  `etc` sections. We add new options `--docdir` and `--etcdir` to both
  Dune's configure and `dune install` command. (#4744, fixes #4723,
  @ejgallego, thanks to @JasonGross for reporting this issue)

- Fix issue where Dune would ignore `(env ... (coq (flags ...)))`
  declarations appearing in `dune` files (#4749, fixes #4566, @ejgallego @rgrinberg)

- Disable some warnings on Coq 8.14 and `(lang coq (>= 0.3))` due to
  the rework of the Coq "native" compilation system (#4760, @ejgallego)

- Fix a bug where instrumentation flags would be added even if the
  instrumentatation was disabled (@nojb, #4770)

- Fix #4682: option `-p` takes now precedence on environement variable
  `DUNE_PROFILE` (#4730, #4774, @bobot, reported by @dra27 #4632)

- Fix installation with opam of package with dune sites. The `.install` file is
  now produced by a local `dune install` during the build phase (#4730, #4645,
  @bobot, reported by @kit-ty-kate #4198)

- Fix multiple issues in the sites feature (#4730, #4645 @bobot, reported by @Lelio-Brun
  #4219, by @Kakadu #4325, by @toots #4415)

2.8.5 (28/03/2021)
------------------

- Fixed absence of executable bit for installed `.cmxs` (#4149, fixes #4148, @bobot)

- Fix a race in Dune cache. It was particularly easy to hit this race when using
  the cache on Windows (#4406, fixes #4167, @snowleopard)

2.8.4 (08/03/2021)
------------------

- Fix crash when META file for `compiler-libs.toplevel` is present
  (@jeremiedimino, #4249)

2.8.3 (07/03/2021)
------------------

- Make `patdiff` show refined diffs (#4257, fixes #4254, @hakuch)

- Fixed a bug that could result in needless recompilation under Windows due to
  case differences in the result of `Sys.getcwd` (observed under `emacs`).
  (#3966, @nojb).

- Restore compatibility with Coq < 8.10 for coq-lang < 0.3 , document
  that `(using coq 0.3)` does require Coq 8.10 at least (#4224, fixes
  #4142, @ejgallego)

- Add a META rule for `compiler-libs.native-toplevel` (#4175, @altgr)

- No longer call `chmod` on symbolic links (fixes #4195, @dannywillems)

- Have `dune` communicate the location of the standard library directory to
  `merlin` (#4211, fixes #4188, @nojb)

- Workaround incorrect exception raised by `Unix.utimes` (OCaml PR#8857) in
  `Path.touch` on Windows. This fixes dune cache in direct mode on Windows.
  (#4223, @dra27)

- `dune ocaml-merlin` is now able to provide configuration for source files in
  the `_build` directory. (#4274, @voodoos)

- Automatically delete left-over Merlin files when rebuilding for the first time
  a project previously built with Dune `<= 2.7`. (#4261, @voodoos, @aalekseyev)

- Fix `ppx.exe` being compiled for the wrong target when cross-compiling
  (#3751, fixes #3698, @toots)

- `dune top` correctly escapes the generated toplevel directives, and make it
  easier for `dune top` to locate C stubs associated to concerned libraries.
  (#4242, fixes #4231, @nojb)

- Do not pass include directories containing native objects when compiling
  bytecode (#4200, @nojb)

- If an .ml file is not used by an executable, Dune no longer report
  parsing error in this file (#4330, @jeremiedimino)

- Experimental ctypes support (#3905, fixes #135, @mbacarella)


2.8.2 (21/01/2021)
------------------

- Fixed wrong workspace discovery from `dune ocaml-merlin` (#4127, fixes #4125,
  @voodoos)

- Fixed memory blow up introduced in 2.8.0 (#4144, fixes #4134,
  @jeremiedimino)

- Configurator: always link the C libraries in the build command
  (#4088, @MisterDA).

2.8.1 (14/01/2021)
------------------

- Fixed `dune --version` printing `n/a` rather than the version

2.8.0 (13/01/2021)
------------------

- `dune rules` accepts aliases and other non-path rules (#4063, @mrmr1993)

- Action `(diff reference test_result)` now accept `reference` to be absent and
  in that case consider that the reference is empty. Then running `dune promote`
  will create the reference file. (#3795, @bobot)

- Ignore special files (BLK, CHR, FIFO, SOCKET), (#3570, fixes #3124, #3546,
  @ejgallego)

- Experimental: Simplify loading of additional files (data or code) at runtime
  in programs by introducing specific installation sites. In particular it allow
  to define plugins to be installed in these sites. (#3104, #3794, fixes #1185,
  @bobot)

- Move all temporary files created by dune to run actions to a single directory
  and make sure that actions executed by dune also use this directory by setting
  `TMPDIR` (or `TEMP` on Windows). (#3691, fixes #3422, @rgrinberg)

- Fix bootstrap script with custom configuration. (#3757, fixes #3774, @marsam)

- Add the `executable` field to `inline_tests` to customize the compilation
  flags of the test runner executable (#3747, fixes #3679, @lubegasimon)

- Add `(enabled_if ...)` to `(copy_files ...)` (#3765, @nojb)

- Make sure Dune cleans up the status line before exiting (#3767,
  fixes #3737, @alan-j-hu)

- Add `{gitlab,bitbucket}` as options for defining project sources with `source`
  stanza `(source (<host> user/repo))` in the `dune-project` file.  (#3813,
  @rgrinberg)

- Fix generation of `META` and `dune-package` files when some targets (byte,
  native, dynlink) are disabled. Previously, dune would generate all archives
  for regardless of settings. (#3829, #4041, @rgrinberg)

- Do not run ocamldep to for single module executables & libraries. The
  dependency graph for such artifacts is trivial (#3847, @rgrinberg)

- Fix cram tests inside vendored directories not being interpreted correctly.
  (#3860, fixes #3843, @rgrinberg)

- Add `package` field to private libraries. This allows such libraries to be
  installed and to be usable by other public libraries in the same project
  (#3655, fixes #1017, @rgrinberg)

- Fix the `%{make}` variable on Windows by only checking for a `gmake` binary
  on UNIX-like systems as a unrelated `gmake` binary might exist on Windows.
  (#3853, @kit-ty-kate)

- Fix `$ dune install` modifying the build directory. This made the build
  directory unusable when `$ sudo dune install` modified permissions. (fix
  #3857, @rgrinberg)

- Fix handling of aliases given on the command line (using the `@` and `@@`
  syntax) so as to correctly handle relative paths. (#3874, fixes #3850, @nojb)

- Allow link time code generation to be used in preprocessing executable. This
  makes it possible to use the build info module inside the preprocessor.
  (#3848, fix #3848, @rgrinberg)

- Correctly call `git ls-tree` so unicode files are not quoted, this fixes
  problems with `dune subst` in the presence of unicode files. Fixes #3219
  (#3879, @ejgallego)

- `dune subst` now accepts common command-line arguments such as
  `--debug-backtraces` (#3878, @ejgallego)

- `dune describe` now also includes information about executables in addition to
  that of libraries. (#3892, #3895, @nojb)

- instrumentation backends can now receive arguments via `(instrumentation
  (backend <name> <args>))`. (#3906, #3932, @nojb)

- Tweak auto-formatting of `dune` files to improve readability. (#3928, @nojb)

- Add a switch argument to opam when context is not default. (#3951, @tmattio)

- Avoid pager when running `$ git diff` (#3912, @AltGr)

- Add `(root_module ..)` field to libraries & executables. This makes it
  possible to use library dependencies shadowed by local modules (#3825,
  @rgrinberg)

- Allow `(formatting ...)` field in `(env ...)` stanza to set per-directory
  formatting specification. (#3942, @nojb)

- [coq] In `coq.theory`, `:standard` for the `flags` field now uses the
  flags set in `env` profile flags (#3931 , @ejgallego @rgrinberg)

- [coq] Add `-q` flag to `:standard` `coqc` flags , fixes #3924, (#3931 , @ejgallego)

- Add support for Coq's native compute compilation mode (@ejgallego, #3210)

- Add a `SUFFIX` directive in `.merlin` files for each dialect with no
  preprocessing, to let merlin know of additional file extensions (#3977,
  @vouillon)

- Stop promoting `.merlin` files. Write per-stanza Merlin configurations in
  binary form. Add a new subcommand `dune ocaml-merlin` that Merlin can use to
  query the configuration files. The `allow_approximate_merlin` option is now
  useless and deprecated. Dune now conflicts with `merlin < 3.4.0` and
  `ocaml-lsp-server < 1.3.0` (#3554, @voodoos)

- Configurator: fix a bug introduced in 2.6.0 where the configurator V1 API
  doesn't work at all when used outside of dune. (#4046, @aalekseyev)

- Fix `libexec` and `libexec-private` variables. In cross-compilation settings,
  they now point to the file in the host context. (#4058, fixes #4057,
  @TheLortex)

- When running `$ dune subst`, use project metadata as a fallback when package
  metadata is missing. We also generate a warning when `(name ..)` is missing in
  `dune-project` files to avoid failures in production builds.

- Remove support for passing `-nodynlink` for executables. It was bypassed in
  most cases and not correct in other cases in particular on arm32.
  (#4085, fixes #4069, fixes #2527, @emillon)

- Generate archive rules compatible with 4.12. Dune no longer attempts to
  generate an archive file if it's unnecessary (#3973, fixes #3766, @rgrinberg)

- Fix generated Merlin configurations when multiple preprocessors are defined
  for different modules in the same folder. (#4092, fixes #2596, #1212 and
  #3409, @voodoos)

- Add the option `use_standard_c_and_cxx_flags` to `dune-project` that 1.
  disables the unconditional use of the `ocamlc_cflags` and `ocamlc_cppflags`
  from `ocamlc -config` in C compiler calls, these flags will be present in the
  `:standard` set instead; and 2. enables the detection of the C compiler family
  and populates the `:standard` set of flags with common default values when
  building CXX stubs. (#3875, #3802, fix #3718 and #3528, @voodoos)

2.7.1 (2/09/2020)
-----------------

- configurator: More flexible probing of `#define`. We allow duplicate values in
  the object file, as long as they are the same after parsing. (#3739, fixes
  #3736, @rgrinberg)

- Record instrumentation backends in dune-package files. This makes it possible
  to use instrumentation backends defined in installed libraries (eg via OPAM).
  (#3735, @nojb)

- Add missing `.aux` & `.glob` targets to coq rules (#3721, fixes #3437,
  @rgrinberg)

- Fix `dune-package` installation when META templates are present (#3743, fixes
  #3746, @rgrinberg)

- Resolve symlinks before running `$ git diff` (#3750, fixes #3740, @rgrinberg)

- Cram tests: when checking that all test directories contain a `run.t` file,
  skip empty directories. These can be left around by git. (#3753, @emillon)

2.7.0 (13/08/2020)
------------------

- Write intermediate files in a `.mdx` folder for each `mdx` stanza
  to prevent the corresponding actions to be executed as part of the `@all`
  alias (#3659, @NathanReb)

- Read Coq flags from `env` (#3547 , fixes #3486, @gares)

- Add instrumentation framework to toggle instrumentation by `bisect_ppx`,
  `landmarks`, etc, via dune-workspace and/or the command-line. (#3404, #3526
  @stephanieyou, @nojb)

- Formatting of dune files is now done in the executing dune process instead of
  in a separate process. (#3536, @nojb)

- Add a `--debug-artifact-substution` flag to help debug problem with
  version not being captured by `dune-build-info` (#3589,
  @jeremiedimino)

- Allow the use of the `context_name` variable in the `enabled_if` fields of
  executable(s) and install stanzas. (#3568, fixes #3566, @voodoos)

- Fix compatibility with OCaml 4.12.0 when compiling empty archives; no .a file
  is generated. (#3576, @dra27)

- `$ dune utop` no longer tries to load optional libraries that are unavailable
  (#3612, fixes #3188, @anuragsoni)

- Fix dune-build-info on 4.10.0+flambda (#3599, @emillon, @jeremiedimino).

- Allow multiple libraries with `inline_tests` to be defined in the same
  directory (#3621, @rgrinberg)

- Run exit hooks in jsoo separate compilation mode (#3626, fixes #3622,
  @rgrinberg)

- Add (alias ...), (mode ...) fields to (copy_fields ...) stanza (#3631, @nojb)

- (copy_files ...) now supports copying files from outside the workspace using
  absolute file names (#3639, @nojb)

- Dune does not use `ocamlc` as an intermediary to call C compiler anymore.
  Configuration flags `ocamlc_cflags` and `ocamlc_cppflags` are always prepended
  to the compiler arguments. (#3565, fixes #3346, @voodoos)

- Revert the build optimization in #2268. This optimization slows down building
  individual executables when they're part of an `executables` stanza group
  (#3644, @rgrinberg)

- Use `{dev}` rather than `{pinned}` in the generated `.opam` file. (#3647,
  @kit-ty-kate)

- Insert correct extension name when editing `dune-project` files. Previously,
  dune would just insert the stanza name. (#3649, fixes #3624, @rgrinberg)

- Fix crash when evaluating an `mdx` stanza that depends on unavailable
  packages. (#3650, @CraigFe)

- Fix typo in `cache-check-probablity` field in dune config files. This field
  now requires 2.7 as it wasn't usable before this version. (#3652, @edwintorok)

- Add `"odoc" {with-doc}` to the dependencies in the generated `.opam` files.
  (#3667, @kit-ty-kate)

- Do not allow user actions to capture dune's stdin (#3677, fixes #3672,
  @rgrinberg)

- `(subdir ...)` stanzas can now appear in dune files used via `(include ...)`.
  (#3676, @nojb)

- Add actions `pipe-{stdout,stderr,outputs}` for output redirections (#3392,
  fixes #428, @NathanReb)

2.6.2 (26/07/2020)
------------------

* Fix compatibility with OCaml 4.12 (#3585, fixes #3583, @ejgallego)

2.6.1 (02/07/2020)
------------------

- Fix crash when caching is enabled (@rgrinberg, #3581, fixes #3580)

- Do not use `-output-complete-exe` until 4.10.1 as it is broken in
  4.10.0 (@jeremiedimino, #3187)

- Fix crash when an unknown pform is found (such as `%{unknown}`) (#3560,
  @emillon)

- Improve error message when invalid package names (such as the empty string)
  are passed to `dune build -p`. (#3561, @emillon)

- Fix a stack overflow when displaying large outputs (including diffs) (#3537,
  fixes #2767, #3490, @emillon)

- Pass `-g` when compiling ppx preprocessors (#3671, @rgrinberg)

2.6.0 (05/06/2020)
------------------

- Fix a bug where valid lib names in `dune init exec --libs=lib1,lib2`
  results in an error. (#3444, fix #3443, @bikallem)

- Add and `enabled_ if` field to the `install` stanza. Enforce the same variable
  restrictions for `enabled_if` fields in the `executable` and `install` stanzas
  than in the `library` stanza. When using dune lang < 2.6, the usage of
  forbidden variables in executables stanzas with only trigger a warning to
  maintain compatibility. (#3408 and #3496, fixes #3354, @voodoos)

- Insert a constraint one the version of dune when the user explicitly
  specify the dependency on dune in the `dune-project` file (#3434 ,
  fixes #3427, @diml)

- Generate correct META files for sub-libraries (of the form `lib.foo`) that
  contain .js runtime files. (#3445, @hhugo)

- Add a `(no-infer ...)` action that prevents inference of targets and
  dependencies in actions. (#3456, fixes #2006, @roddyyaga)

- Correctly infer targets for the `diff?` action. (#3457, fixes #2990, @greedy)

- Fix `$ dune print-rules` crashing (#3459, fixes #3440, @rgrinberg)

- Simplify js_of_ocaml rules using js_of_ocaml.3.6 (#3375, @hhugo)

- Add a new `ocaml-merlin` subcommand that can be used by Merlin to get
  configuration directly from dune instead of using `.merlin` files. (#3395,
  @voodoos)

- Remove experimental variants feature and make default implementations part of
  the language (#3491, fixes #3483, @rgrinberg)

2.5.1 (17/04/2020)
------------------

- [coq] Fix install .v files for Coq theories (#3384, @lthms)

- [coq] Fix install path for theory names with level greater than 1 (#3358,
  @ejgallego)

- Fix a bug introduced in 2.0.0 where the [locks] field in rules with no targets
  had no effect. (@aalekseyev, report by @craigfe)

2.5.0 (09/04/2020)
------------------

- Add a `--release` option meaning the same as `-p` but without the
  package filtering. This is useful for custom `dune` invocation in opam
  files where we don't want `-p` (#3260, @diml)

- Fix a bug introduced in 2.4.0 causing `.bc` programs to be built
  with `-custom` by default (#3269, fixes #3262, @diml)

- Allow contexts to be defined with local switches in workspace files (#3265,
  fix #3264, @rgrinberg)

- Delay expansion errors until the rule is used to build something (#3261, fix
  #3252, @rgrinberg, @diml)

- [coq] Support for theory dependencies and compositional builds using
  new field `(theories ...)` (#2053, @ejgallego, @rgrinberg)

- From now on, each version of a syntax extension must be explicitely tied to a
  minimum version of the dune language. Inconsistent versions in a
  `dune-project` will trigger a warning for version <=2.4 and an error for
  versions >2.4 of the dune language. (#3270, fixes #2957, @voodoos)

- [coq] Bump coq lang version to 0.2. New coq features presented this release
  require this version of the coq lang. (#3283, @ejgallego)

- Prevent installation of public executables disabled using the `enabled_if` field.
  Installation will now simply skip such executables instead of raising an
  error. (#3195, @voodoos)

- `dune upgrade` will now try to upgrade projects using versions <2.0 to version
  2.0 of the dune language. (#3174, @voodoos)

- Add a `top` command to integrate dune with any toplevel, not just
  utop. It is meant to be used with the new `#use_output` directive of
  OCaml 4.11 (#2952, @mbernat, @diml)

- Allow per-package `version` in generated `opam` files (#3287, @toots)

- [coq] Introduce the `coq.extraction` stanza. It can be used to extract OCaml
  sources (#3299, fixes #2178, @rgrinberg)

- Load ppx rewriters in dune utop and add pps field to toplevel stanza. Ppx
  extensions will now be usable in the toplevel
  (#3266, fixes #346, @stephanieyou)

- Add a `(subdir ..)` stanza to allow evaluating stanzas in sub directories.
  (#3268, @rgrinberg)

- Fix a bug preventing one from running inline tests in multiple modes
  (#3352, @diml)

- Allow the use of the `%{profile}` variable in the `enabled_if` field of the
  library stanza. (#3344, @mrmr1993)

- Allow the use of `%{ocaml_version}` variable in `enabled_if` field of the
  library stanza. (#3339, @voodoos)

- Fix dune build freezing on MacOS when cache is enabled. (#3249, fixes ##2973,
  @artempyanykh)

2.4.0 (06/03/2020)
------------------

- Add `mdx` extension and stanza version 0.1 (#3094, @NathanReb)

- Allow to make Odoc warnings fatal. This is configured from the `(env ...)`
  stanza. (#3029, @Julow)

- Fix separate compilation of JS when findlib is not installed. (#3177, @nojb)

- Add a `dune describe` command to obtain the topology of a dune workspace, for
  projects such as ROTOR. (#3128, @diml)

- Add `plugin` linking mode for executables and the `(embed_in_plugin_libraries
  ...)` field. (#3141, @nojb)

- Add an `%{ext_plugin}` variable (#3141, @nojb)

- Dune will no longer build shared objects for stubs if
  `supports_shared_libraries` is false (#3225, fixes #3222, @rgrinberg)

- Fix a memory leak in the file-watching mode (`dune build -w`)
  (#3220, @snowleopard and @aalekseyev)

- Starting from `(lang dune 2.4)`, dune systematically puts all files
  under `_build` in read-only mode instead of only doing it when the
  shared cache is enabled (#3092, @mefyl)

2.3.1 (20/02/2020)
------------------

- Fix versioning of artifact variables (eg %{cmxa:...}), which were introduced
  in 2.0, not 1.11. (#3149, @nojb)

- Fix a bug introduced in 2.3.0 where dune insists on using `fswatch` on linux
  (even when `inotifywait` is available). (#3162, @aalekseyev)

- Fix a bug causing all executables to be considered as optional (#3163, @diml)

2.3.0 (15/02/2020)
------------------

- Improve validation and error handling of arguments to `dune init` (#3103, fixes
  #3046, @shonfeder)

- `dune init exec NAME` now uses the `NAME` argument for private modules (#3103,
  fixes #3088, @shonfeder)

- Avoid linear walk to detect children, this should greatly improve
  performance when a target has a large number of dependencies (#2959,
  @ejgallego, @aalekseyev, @Armael)

- [coq] Add `(boot)` option to `(coq.theories)` to enable bootstrap of
  Coq's stdlib (#3096, @ejgallego)

- [coq] Deprecate `public_name` field in favour of `package` (#2087, @ejgallego)

- Better error reporting for "data only" and "vendored" dirs. Using these with
  anything else than a strict subdirectory or `*` will raise an error. The
  previous behavior was to just do nothing  (#3056, fixes #3019, @voodoos)

- Fix bootstrap on bytecode only switches on windows or where `-j1` is set.
  (#3112, @xclerc, @rgrinberg)

- Allow `enabled_if` fields in `executable(s)` stanzas (#3137, fixes #1690
  @voodoos)

- Do not fail if `ocamldep`, `ocamlmklib`, or `ocaml` are absent. Wait for them
  to be used to fail (#3138, @rgrinberg)

- Introduce a `strict_package_deps` mode that verifies that dependencies between
  packages in the workspace are specified correctly. (@rgrinberg, #3117)

- Make sure the `@all` alias is defined when no `dune` file is present
  in a directory (#2946, fix #2927, @diml)

2.2.0 (06/02/2020)
------------------

- `dune test` is now a command alias for `dune runtest`. This is to make the CLI
  less idiosyncratic (#3006, @shonfeder)

- Allow to set menhir flags in the `env` stanza using the `menhir_flags` field.
  (#2960, fix #2924, @bschommer)

- By default, do not show the full command line of commands executed
  by `dune` when `dune` is executed inside `dune`. This is to make
  integration tests more reproducible (#3042, @diml)

- `dune subst` now works even without opam files (#2955, fixes #2910,
  @fangyi-zhou and @diml)

- Hint when trying to execute an executable defined in the current directory
  without using the `./` prefix (#3041, fixes #1094, @voodoos).

- Extend the list of modifiers that can be nested under
  `with-accepted-exit-codes` with `chdir`,  `setenv`, `ignore-<outputs>`,
  `with-stdin-from` and `with-<outputs>-to` (#3027, fixes #3014, @voodoos)

- It is now an error to have a preprocessing dependency on a ppx rewriter
  library that is not marked as `(kind ppx_rewriter)` (#3039, @snowleopard).

- Fix permissions of files promoted to the source tree when using the
  shared cache. In particular, make them writable by the user (#3043,
  fixes #3026, @diml)

- Only detect internal OCaml tools with `.opt` extensions. Previously, this
  detection applied to other binaries as well (@kit-ty-kate, @rgrinberg, #3051).

- Give the user a proper error message when they try to promote into a source
  directory that doesn't exist. (#3073, fix #3069, @rgrinberg)

- Correctly build vendored packages in `-p` mode. These packages were
  incorrectly filtered out before. (#3075, @diml)

- Do not install vendored packages (#3074, @diml)

- `make` now prints a message explaining the main targets available
  (#3085, fix #3078, @diml)

- Add a `byte_complete` executable mode to build programs as
  self-contained bytecode programs
  (#3076, fixes #1519, @diml)

2.1.3 (16/01/2020)
------------------

- Fix building the OCaml compiler with Dune (#3038, fixes #2974,
  @diml)

2.1.2 (08/01/2020)
------------------

- Fix a bug in the `Fiber.finalize` function of the concurrency monad of Dune,
  causing a race condition at the user level (#3009, fix #2958, @diml)

2.1.1 (07/01/2020)
------------------

- Guess foreign archives & native archives for libraries defined using the
  `META` format. (#2994, @rgrinberg, @anmonteiro)

- Fix generation of `.merlin` files when depending on local libraries with more
  than one source directory. (#2983, @rgrinberg)

2.1.0 (21/12/2019)
------------------

- Attach cinaps stanza actions to both `@runtest` and `@cinaps` aliases
  (#2831, @NathanReb)

- Add variables `%{lib-private...}` and `%{libexec-private...}` for finding
  build paths of files in public and private libraries within the same
  project. (#2901, @snowleopard)

- Add `--mandir` option to `$ dune install`. This option allows to override the
  installation directory for man pages. (#2915, fixes #2670, @rgrinberg)

- Fix `dune --version`. The bootstrap didn't compute the version
  correctly. (#2929, fixes #2911, @diml)

- Do not open the log file in `dune clean`. (#2965, fixes #2964 and
  #2921, @diml)

- Support passing two arguments to `=`, `<>`, ... operators in package
  dependencies so that we can have things such as `(<> :os win32)`
  (#2965, @diml)

2.0.1 (17/12/2019)
------------------

- Delay errors raised by invalid `dune-package` files. The error is now raised
  only if the invalid package is treated as a library and used to build
  something. (#2972, @rgrinberg)

2.0.0 (20/11/2019)
------------------

- Remove existing destination files in `install`  before installing the new
  ones. (#2885, fixes #2883, @bschommer)

- The `action` field in the `alias` stanza is not available starting `lang dune
  2.0`. The `alias` field in the `rule` stanza is a replacement. (#2846, fixes
  2681, @rgrinberg)

- Introduce `alias` and `package` fields to the `rule` stanza. This is the
  preferred way of attaching rules to aliases. (#2744, @rgrinberg)

- Add field `(optional)` for executable stanzas (#2463, fixes #2433, @bobot)

- Infer targets for rule stanzas expressed in long form (#2494, fixes #2469,
  @NathanReb)

- Indicate the progress of the initial file tree loading (#2459, fixes #2374,
  @bobot)

- Build `.cm[ox]` files for executables more eagerly. This speeds up builds at
  the cost of building unnecessary artifacts in some cases. Some of these extra
  artifacts can fail to built, so this is a breaking change. (#2268, @rgrinberg)

- Do not put the `<package>.install` files in the source tree unless `-p` or
  `--promote-install-files` is passed on the command line (#2329, @diml)

- Compilation units of user defined executables are now mangled by default. This
  is done to prevent the accidental collision with library dependencies of the
  executable. (#2364, fixes #2292, @rgrinberg)

- Enable `(explicit_js_mode)` by default. (#1941, @nojb)

- Add an option to clear the console in-between builds with
 `--terminal-persistence=clear-on-rebuild`

- Stop symlinking object files to main directory for stanzas defined `jbuild`
  files (#2440, @rgrinberg)

- Library names are now validated in a strict fashion. Previously, invalid names
  would be allowed for unwrapped libraries (#2442, @rgrinberg)

- mli only modules must now be explicitly declared. This was previously a
  warning and is now an error. (#2442, @rgrinberg)

- Modules filtered out from the module list via the Ordered Set Language must
  now be actual modules. (#2442, @rgrinberg)

- Actions which introduce targets where new targets are forbidden (e.g.
  preprocessing) are now an error instead of a warning. (#2442, @rgrinberg)

- No longer install a `jbuilder` binary. (#2441, @diml)

- Stub names are no longer allowed relative paths. This was previously a warning
  and is now an error (#2443, @rgrinberg).

- Define (paths ...) fields in (context ...) definitions in order to set or
  extend any PATH-like variable in the context environment. (#2426, @nojb)

- The `diff` action will always normalize newlines before diffing. Perviousy, it
  would not do this normalization for rules defined in jbuild files. (#2457,
  @rgrinberg)

- Modules may no longer belong to more than one stanza. This was previously
  allowed only in stanzas defined in `jbuild` files. (#2458, @rgrinberg)

- Remove support for `jbuild-ignore` files. They have been replaced by the the
  `dirs` stanza in `dune` files. (#2456, @rgrinberg)

- Add a new config option `sandboxing_preference`, the cli argument `--sandbox`,
  and the dep spec `sandbox` in dune language. These let the user control the
  level of sandboxing done by dune per rule and globally. The rule specification
  takes precedence. The global configuration merely specifies the default.
  (#2213, @aalekseyev, @diml)

- Remove support for old style subsystems. Dune will now emit a warning to
  reinstall the library with the old style subsystem. (#2480, @rgrinberg)

- Add action (with-stdin-from <file> <action>) to redirect input from <file>
  when performing <action>. (#2487, @nojb)

- Change the automatically generated odoc index to only list public modules.
  This only affects unwrapped libraries (#2479, @rgrinberg)

- Set up formatting rules by default. They can be configured through a new
  `(formatting)` stanza in `dune-project` (#2347, fixes #2315, @emillon)

- Change default target from `@install` to `@all`. (#2449, fixes #1220,
  @rgrinberg)

- Include building stubs in `@check` rules. (@rgrinberg, #2530)

- Get rid of ad-hoc rules for guessing the version. Dune now only
  relies on the version written in the `dune-project` file and no
  longer read `VERSION` or similar files (#2541, @diml)

- In `(diff? x y)` action, require `x` to exist and register a
  dependency on that file. (#2486, @aalekseyev)

- On Windows, an .exe suffix is no longer added implicitly to binary names that
  already end in .exe. Second, when resolving binary names, .opt variants are no
  longer chosen automatically. (#2543, @nojb)

- Make `(diff? x y)` move the correction file (`y`) away from the build
  directory to promotion staging area. This makes corrections work with
  sandboxing and in general reduces build directory pollution. (#2486,
  @aalekseyev, fixes #2482)

- `c_flags`, `c_names` and `cxx_names` are now supported in `executable` and
  `executables` stanzas. (#2562, @nojb) Note: this feature has been subsequently
  extended into a separate `foreign_stubs` field. The fields `c(xx)_names` and
  `c(xx)_flags` are now deleted. (#2659, RFC #2650, @snowleopard)

- Remove git integration from `$ dune upgrade` (#2565, @rgrinberg)

- Add a `--disable-promotion` to disable all modification to the source
  directory. There's also a corresponding `DUNE_DISABLE_PROMOTION` environment
  variable. (#2588, fix #2568, @rgrinberg)

- Add a `forbidden_libraries` field to prevent some library from being
  linked in an executable. This help detecting who accidently pulls in
  `unix` for instance (#2570, @diml)

- Fix incorrect error message when a variable is expanded in static context:
  `%{lib:lib:..}` when the library does not exist. (#2597, fix #1541,
  @rgrinberg)

- Add `--sections` option to `$ dune install` to install subsections of .install
  files. This is useful for installing only the binaries in a workspace for
  example. (#2609, fixes #2554, @rgrinberg)

- Drop support for `jbuild` and `jbuild-ignore` files (#2607, @diml)

- Add a `dune-action-plugin` library for describing dependencies direcly in
  the executable source. Programs that use this feature can be run by a new
  action (dynamic-run <progn> ...). (#2635, @staronj, @aalekseyev)

- Stop installing the `ocaml-syntax-shims` binary. In order to use
  `future_syntax`, one now need to depend on the `ocaml-syntax-shims`
  package (#2654, @diml)

- Add support for dependencies that are re-exported. Such dependencies
  are marked with`re_export` and will automatically be provided to
  users of a library (#2605, @rgrinberg)

- Add a `deprecated_library_name` stanza to redirect old names after a
  library has been renamed (#2528, @diml)

- Error out when a `preprocessor_deps` field is present but not
  `preprocess` field is. It is a warning with Dune 1.x projects
  (#2660, @Julow)

- Dune will use `-output-complete-exe` instead of `-custom` when compiling
  self-contained bytecode executables whenever this options is available
  (OCaml version >= 4.10) (#2692, @nojb)

- Add action `(with-accepted-exit-codes <pred> <action>)` to specify the set of
  successful exit codes of `<action>`. `<pred>` is specified using the predicate
  language. (#2699, @nojb)

- Do not setup rules for disabled libraries (#2491, fixes #2272, @bobot)

- Configurator: filter out empty flags from `pkg-config` (#2716, @AltGr)

- `no_keep_locs` is a no-op for projects that use `lang dune` older than 2.0. In
  projects where the language is at least `2.0`, the field is now forbidden.
  (#2752, fixes #2747, @rgrinberg)

- Extend support for foreign sources and archives via the `(foreign_library ...)`
  stanza as well as the `(foreign_stubs ...)` and `(foreign_archives ...)` fields.
  (#2659, RFC #2650, @snowleopard)

- Add (deprecated_package_names) field to (package) declaration in
  dune-project. The names declared here can be used in the (old_public_name)
  field of (deprecated_library_name) stanza. These names are interpreted as
  library names (not prefixed by a package name) and appropiate redirections are
  setup in their META files. This feaure is meant to migrate old libraries which
  do not follow Dune's convention of prefixing libraries with the package
  name. (#2696, @nojb)

- The fields `license`, `authors`, `maintainers`, `source`, `bug_reports`,
  `homepage`, and `documentation` of `dune-project` can now be overriden on a
  per-package basis. (#2774, @nojb)

- Change the default `modes` field of executables to `(mode exe)`. If
  one wants to build a bytecode program, it now needs to be explicitly
  requested via `(modes byte exe)`. (#2851, @diml)

- Allow `ccomp_type` as a variable for evaluating `enabled_if`. (#2855, @dra27,
  @rgrinberg)

- Stricter validation of file names in `select`. The file names of conditional
  sources must match the prefix and the extension of the resultant filename.
  (#2867, @rgrinberg)

- Add flag `disable_dynamically_linked_foreign_archives` to the workspace file.
  If the flag is set to `true` then: (i) when installing libraries, we do not
  install dynamic foreign archives `dll*.so`; (ii) when building executables in
  the `byte` mode, we statically link in foreign archives into the runtime
  system; (iii) we do not generate any `dll*.so` rules. (#2864, @snowleopard)

- Reimplement the bootstrap procedure. The new procedure is faster and
  should no longer stack overflow (#2854, @dra27, @diml)

- Allow `.opam.template` files to be generated using rules (#2866, @rgrinberg)

- Delete the deprecated `self_build_stubs_archive` field, replaced by
  `foreign_archives`.

1.11.4 (09/10/2019)
-------------------

- Allow to mark directories as `data_only_dirs` without including them as `dirs`
  (#2619, fix #2584, @rgrinberg)

- Fix reading `.install` files generated with an external `--build-dir`. (#2638,
  fix #2629, @rgrinberg)

1.11.3 (23/08/2019)
-------------------

- Fix a ppx hash collision in watch mode (#2546, fixes #2520, @diml)

1.11.2 (20/08/2019)
-------------------

- Remove the optimisation of passing `-nodynlink` for executalbes when
  not necessary. It seems to be breaking things (see #2527, @diml)

- Fix invalid library names in `dune-package` files. Only public names should
  exist in such files. (#2558, fix #2425, @rgrinberg)

1.11.1 (09/08/2019)
-------------------

- Fix config file dependencies of ocamlformat (#2471, fixes #2464,
  @nojb)

- Cleanup stale directories when using `(source_tree ...)` in the
  presence of directories with only sub-directories and no files
  (#2514, fixes #2499, @diml)

1.11.0 (23/07/2019)
-------------------

- Don't select all local implementations in `dune utop`. Instead, let the
  default implementation selection do its job. (#2327, fixes #2323, @TheLortex,
  review by @rgrinberg)

- Check that selected implementations (either by variants or default
  implementations) are indeed implementations. (#2328, @TheLortex, review by
  @rgrinberg)

- Don't reserve the `Ppx` toplevel module name for ppx rewriters (#2242, @diml)

- Redesign of the library variant feature according to the #2134 proposal. The
  set of variants is now computed when the virtual library is installed.
  Introducing a new `external_variant` stanza. (#2169, fixes #2134, @TheLortex,
  review by @diml)

- Add proper line directives when copying `.cc` and `.cxx` sources (#2275,
  @rgrinberg)

- Fix error message for missing C++ sources. The `.cc` extension was always
  ignored before. (#2275, @rgrinberg)

- Add `$ dune init project` subcommand to create project boilerplate according
  to a common template. (#2185, fixes #159, @shonfeder)

- Allow to run inline tests in javascript with nodejs (#2266, @hhugo)

- Build `ppx.exe` as compiling host binary. (#2286, fixes #2252, @toots, review
  by @rgrinberg and @diml)

- Add a `cinaps` extension and stanza for better integration with the
  [cinaps tool](https://github.com/janestreet/cinaps) tool (#2269,
  @diml)

- Allow to embed build info in executables such as version and list
  and version of statically linked libraries (#2224, @diml)

- Set version in `META` and `dune-package` files to the one read from
  the vcs when no other version is available (#2224, @diml)

- Add a variable `%{target}` to be used in situations where the context
  requires at most one word, so `%{targets}` can be confusing; stdout
  redirections and "-o" arguments of various tools are the main use
  case; also, introduce a separate field `target` that must be used
  instead of `targets` in those situations.  (#2341, @aalekseyev)

- Fix dependency graph of wrapped_compat modules. Previously, the dependency on
  the user written entry module was omitted. (#2305, @rgrinberg)

- Allow to promote executables built with an `executable` stanza
  (#2379, @diml)

- When instantiating an implementation with a variant, make sure it matches
  virtual library's list of known implementations. (#2361, fixes #2322,
  @TheLortex, review by @rgrinberg)

- Add a variable `%{ignoring_promoted_rules}` that is `true` when
  `--ingore-promoted-rules` is passed on the command line and false
  otherwise (#2382, @diml)

- Fix a bug in `future_syntax` where the characters `@` and `&` were
  not distinguished in the names of binding operators (`let@` was the
  same as `let&`) (#2376, @aalekseyev, @diml)

- Workspaces with non unique project names are now supported. (#2377, fix #2325,
  @rgrinberg)

- Improve opam generation to include the `dune` dependencies with the minimum
  constraint set based on the dune language version specified in the
  `dune-project` file. (2383, @avsm)

- The order of fields in the generated opam file now follows order preferred in
  opam-lib. (@avsm, #2380)

- Fix coloring of error messages from the compiler (@diml, #2384)

- Add warning `66` to default set of warnings starting for dune projects with
  language verison >= `1.11` (@rgrinberg, @diml, fixes #2299)

- Add (dialect ...) stanza
  (@nojb, #2404)

- Add a `--context` argument to `dune install/uninstall` (@diml, #2412)

- Do not warn about merlin files pre 1.9. This warning can only be disabled in
  1.9 (#2421, fixes #2399, @emillon)

- Add a new `inline_tests` field in the env stanza to control inline_tests
  framework with a variable (#2313, @mlasson, original idea by @diml, review
  by @rgrinberg).

- New binary kind `js` for executables in order to explicitly enable Javascript
  targets, and a switch `(explicit_js_mode)` to require this mode in order to
  declare JS targets corresponding to executables. (#1941, @nojb)

1.10.0 (04/06/2019)
-------------------

- Restricted the set of variables available for expansion in the destination
  filename of `install` stanza to simplify implementation and avoid dependency
  cycles. (#2073, @aalekseyev, @diml)

- [menhir] call menhir from context root build_dir (#2067, @ejgallego,
  review by @diml, @rgrinberg)

- [coq] Add `coq.pp` stanza to help with pre-processing of grammar
  files (#2054, @ejgallego, review by @rgrinberg)

- Add a new more generic form for the *promote* mode: `(promote
  (until-clean) (into <dir>))` (#2068, @diml)

- Allow to promote only a subset of the targets via `(promote (only
  <pred>))`. For instance: `(promote (only *.mli))` (#2068, @diml)

- Improve the behavior when a strict subset of the targets of a rule is already
  in the source tree for projects using the dune language < 1.10 (#2068, fixes
  #2061, @diml)

- With lang dune >= 1.10, rules in standard mode are no longer allowed to
  produce targets that are present in the source tree. This has been a warning
  for long enough (#2068, @diml)

- Allow %{...} variables in pps flags (#2076, @mlasson review by @diml and
  @aalekseyev).

- Add a 'cookies' option to ppx_rewriter/deriver flags in library stanzas. This
  allow to specify cookie requests from variables expanded at each invocation of
  the preprocessor. (#2106, @mlasson @diml)

- Add more opam metadata and use it to generate `.opam` files. In particular, a
  `package` field has been added to specify package specific information.
  (#2017, #2091, @avsm, @jonludlam, @rgrinberg)

- Clean up the special support for `findlib.dynload`. Before, Dune would simply
  match on the library name. Now, we only match on the findlib package name when
  the library doesn't come from Dune. Someone writing a library called
  `findlib.dynload` with Dune would have to add `(special_builtin_support
  findlib_dynload)` to trigger the special behavior. (#2115, @diml)

- Install the `future_syntax` preprocessor as `ocaml-syntax-shims.exe` (#2125,
  @rgrinberg)

- Hide full command on errors and warnings in development and show them in CI.
  (detected using the `CI` environment variable). Commands for which the
  invocation might be omitted must output an error prefixed with `File `. Add an
  `--always-show-command-line` option to disable this behavior and always show
  the full command. (#2120, fixes #1733, @rgrinberg)

- In `dune-workspace` files, add the ability to choose the host context and to
  create duplicates of the default context with different settings. (#2098,
  @TheLortex, review by @diml, @rgrinberg and @aalekseyev)

- Add support for hg in `dune subst` (#2135, @diml)

- Don't build documentation for implementations of virtual libraries (#2141,
  fixes #2138, @jonludlam)

- Fix generation of the `-pp` flag in .merlin (#2142, @rgrinberg)

- Make `dune subst` add a `(version ...)` field to the `dune-project`
  file (#2148, @diml)

- Add the `%{os_type}` variable, which is a short-hand for
  `%{ocaml-config:os_type}` (#1764, @diml)

- Allow `enabled_if` fields in `library` stanzas, restricted to the
  `%{os_type}`, `%{model}`, `%{architecture}`, `%{system}` variables (#1764,
  #2164 @diml, @rgrinberg)

- Fix `chdir` on external and source paths. Dune will also fail gracefully if
  the external or source path does not exist (#2165, fixes #2158, @rgrinberg)

- Support the `.cc` extension fro C++ sources (#2195, fixes #83, @rgrinberg)

- Run `ocamlformat` relative to the context root. This improves the locations of
  errors. (#2196, fixes #1370, @rgrinberg)

- Fix detection of `README`, `LICENSE`, `CHANGE`, and `HISTORY` files. These
  would be undetected whenever the project was nested in another workspace.
  (#2194, @rgrinberg)

- Fix generation of `.merlin` whenever there's more than one stanza with the
  same ppx preprocessing specification (#2209 ,fixes #2206, @rgrinberg)

- Fix generation of `.merlin` in the presence of the `copy_files` stanza and
  preprocessing specifications of other stanazs. (#2211, fixes #2206,
  @rgrinberg)

- Run `refmt` from the context's root directory. This improves error messages in
  case of syntax errors. (#2223, @rgrinberg)

- In .merlin files, don't pass `-dump-ast` to the `future_syntax` preprocessor.
  Merlin doesn't seem to like it when binary AST is generated by a `-pp`
  preprocessor. (#2236, @aalekseyev)

- `dune install` will verify that all files mentioned in all .install files
  exist before trying to install anything. This prevents partial installation of
  packages (#2230, @rgrinberg)

1.9.3 (06/05/2019)
------------------

- Fix `.install` files not being generated (#2124, fixes #2123, @rgrinberg)

1.9.2 (02/05/2019)
------------------

- Put back library variants in development mode. We discovered a
  serious unexpected issue and we might need to adjust the design of
  this feature before we are ready to commit to a final version. Users
  will need to write `(using library_variants 0.1)` in their
  `dune-project` file if they want to use it before the design is
  finalized. (#2116, @diml)

- Forbid to attach a variant to a library that implements a virtual
  library outside the current project (#2104, @rgrinberg)

- Fix a bug where `dune install` would install man pages to incorrect
  paths when compared to `opam-installer`. For example dune now
  installs `(foo.1 as man1/foo.1)` correctly and previously that was
  installed to `man1/man1/foo.1`. (#2105, @aalekseyev)

- Do not fail when a findlib directory doesn't exist (#2101, fix #2099, @diml)

- [coq] Rename `(coqlib ...)` to `(coq.theory ...)`, support for
  `coqlib` will be dropped in the 1.0 version of the Coq language
  (#2055, @ejgallego)

- Fix crash when calculating library dependency closure (#2090, fixes #2085,
  @rgrinberg)

- Clean up the special support for `findlib.dynload`. Before, Dune
  would simply match on the library name. Now, we only match on the
  findlib package name when the library doesn't come from
  Dune. Someone writing a library called `findlib.dynload` with Dune
  would have to add `(special_builton_support findlib_dynload)` to
  trigger the special behavior. (#2115, @diml)

- Include permissions in the digest of targets and dependencies (#2121, fix
  #1426, @rgrinberg, @xclerc)

1.9.1 (11/04/2019)
------------------

- Fix invocation of odoc to add previously missing include paths, impacting
  mld files that are not in directories containing libraries (#2016, fixes
  #2007, @jonludlam)

1.9.0 (09/04/2019)
------------------

- Warn when generated `.merlin` does not reflect the preprocessing
  specification. This occurs when multiple stanzas in the same directory use
  different preprocessing specifications. This warning can now be disabled with
  `allow_approx_merlin` (#1947, fix #1946, @rgrinberg)

- Watch mode: display "Success" in green and "Had errors" in red (#1956,
  @emillon)

- Fix glob dependencies on installed directories (#1965, @rgrinberg)

- Add support for library variants and default implementations. (#1900,
  @TheLortex)

- Add experimental `$ dune init` command. This command is used to create or
  update project boilerplate. (#1448, fixes #159, @shonfeder)

- Experimental Coq support (fix #1466, @ejgallego)

- Install .cmi files of private modules in a `.private` directory (#1983, fix
  #1973 @rgrinberg)

- Fix `dune subst` attempting to substitute on directories. (#2000, fix #1997,
  @rgrinberg)

- Do not list private modules in the generated index. (#2009, fix #2008,
  @rgrinberg)

- Warn instead of failing if an opam file fails to parse. This opam file can
  still be used to define scope. (#2023, @rgrinberg)

- Do not crash if unable to read a directory when traversing to find root
  (#2024, @rgrinberg)

- Do not exit dune if some source directories are unreadable. Instead, warn the
  user that such directories need to be ignored (#2004, fix #310, @rgrinberg)

- Fix nested `(binaries ..)` fields in the `env` stanza. Previously, parent
  `binaries` fields would be ignored, but instead they should be combined.
  (#2029, @rgrinberg)

- Allow "." in `c_names` and `cxx_names` (#2036, fix #2033, @rgrinberg)

- Format rules: if a dune file uses OCaml syntax, do not format it.
  (#2014, fix #2012, @emillon)

1.8.2 (10/03/2019)
------------------

- Fix auto-generated `index.mld`. Use correct headings for the listing. (#1925,
  @rgrinberg, @aantron)

1.8.1 (08/03/2019)
------------------

- Correctly write `dune-package` when version is empty string (#1919, fix #1918,
  @rgrinberg)

1.8.0 (07/03/2019)
------------------

- Clean up watch mode polling loop: improves signal handling and error handling
  during polling (#1912, fix #1907, fix #1671, @aalekseyev)

- Change status messages during polling to be one-line, so that the messages are
  correctly erased by ^K. (#1912, @aalekseyev)

- Add support for `.cxx` extension for C++ stubs (#1831, @rgrinberg)

- Add `DUNE_WORKSPACE` variable. This variable is equivalent to setting
  `--workspace` in the command line. (#1711, fix #1503, @rgrinberg)

- Add `c_flags` and `cxx_flags` to env profile settings (#1700 and #1800,
  @gretay-js)

- Format `dune printenv` output (#1867, fix #1862, @emillon)

- Add the `(promote-into <dir>)` and `(promote-until-clean-into
  <dir>)` modes for `(rule ...)` stanzas, so that files can be
  promoted in another directory than the current one. For instance,
  this is used in merlin to promote menhir generated files in a
  directory that depends on the version of the compiler (#1890, @diml)

- Improve error message when `dune subst` fails (#1898, fix #1897, @rgrinberg)

- Add more GC counters to catapult traces (fix908, @rgrinberg)

- Add a preprocessor shim for the `let+` syntax of OCaml 4.08 (#1899,
  implements #1891, @diml)

- Fix generation of `.merlin` files on Windows. `\` characters needed
  to be escaped (#1869, @mlasson)

- Fix 0 error code when `$ dune format-dune-file` fails. (#1915, fix #1914,
  @rgrinberg)

- Configurator: deprecated `query_expr` and introduced `query_expr_err` which is
  the same but with a better error in case it fails. (#1886, @ejgallego)

- Make sure `(menhir (mode promote) ...)` stanzas are ignored when
  using `--ignore-promoted-rules` or `-p` (#1917, @diml)

1.7.3 (27/02/2019)
------------------

- Fix interpretation of `META` files containing archives with `/` in
  the filename. For instance, this was causing llvm to be unusable
  with dune (#1889, fix #1885, @diml)

- Make errors about menhir stanzas be located (#1881, fix #1876,
  @diml)

1.7.2 (21/02/2019)
------------------

- Add `${corrected-suffix}`, `${library-name}` and a few other
  variables to the list of variables to upgrade. This fixes the
  support for various framework producing corrections (#1840, #1853,
  @diml)

- Fix `$ dune subst` failing because the build directory wasn't set. (#1854, fix
  #1846, @rgrinberg)

- Configurator: Add warning to `Pkg_config.query` when a full package expression
  is used. Add `Pkg_config.query_expr` for cases when the full power of
  pkg-config's querying is needed (#1842, fix #1833, @rgrinberg)

- Fix unavailable, optional implementations eagerly breaking the build (#1857,
  fix #1856, @rgrinberg)

1.7.1 (13/02/2019)
------------------

- Fix the watch mode (#1837, #1839, fix #1836, @diml)

- Configurator: Fix misquoting when running pkg-config (#1835, fix #1833,
  @Chris00)

1.7.0 (12/02/2019)
------------------


- Second step of the deprecation of jbuilder: the `jbuilder` binary
  now emits a warning on every startup and both `jbuilder` and `dune`
  emit warnings when encountering `jbuild` files (#1752, @diml)

- Change the layout of build artifacts inside _build. The new layout enables
  optimizations that depend on the presence of `.cmx` files of private modules
  (#1676, @bobot)

- Fix merlin handling of private module visibility (#1653 @bobot)

- unstable-fmt: use boxes to wrap some lists (#1608, fix #1153, @emillon,
  thanks to @rgrinberg)

- skip directories when looking up programs in the PATH (#1628, fixes
  #1616, @diml)

- Use `lsof` on macOS to implement `--stats` (#1636, fixes #1634, @xclerc)

- Generate `dune-package` files for every package. These files are installed and
  read instead of `META` files whenever they are available (#1329, @rgrinberg)

- Fix preprocessing for libraries with `(include_subdirs ..)` (#1624, fix #1626,
  @nojb, @rgrinberg)

- Do not generate targets for archive that don't match the `modes` field.
  (#1632, fix #1617, @rgrinberg)

- When executing actions, open files lazily and close them as soon as
  possible in order to reduce the maximum number of file descriptors
  opened by Dune (#1635, #1643, fixes #1633, @jonludlam, @rgrinberg,
  @diml)

- Reimplement the core of Dune using a new generic memoization system
  (#1489, @rudihorn, @diml)

- Replace the broken cycle detection algorithm by a state of the art
  one from [this paper](https://doi.org/10.1145/2756553) (#1489,
  @rudihorn)

- Get the correct environment node for multi project workspaces (#1648,
  @rgrinberg)

- Add `dune compute` to call internal memoized functions (#1528,
  @rudihorn, @diml)

- Add `--trace-file` option to trace dune internals (#1639, fix #1180, @emillon)

- Add `--no-print-directory` (borrowed from GNU make) to suppress
  `Entering directory` messages. (#1668, @dra27)

- Remove `--stats` and track fd usage in `--trace-file` (#1667, @emillon)

- Add virtual libraries feature and enable it by default (#1430 fixes #921,
  @rgrinberg)

- Fix handling of Control+C in watch mode (#1678, fixes #1671, @diml)

- Look for jsoo runtime in the same dir as the `js_of_ocaml` binary
  when the ocamlfind package is not available (#1467, @nojb)

- Make the `seq` package available for OCaml >= 4.07 (#1714, @rgrinberg)

- Add locations to error messages where a rule fails to generate targets and
  rules that require files outside the build/source directory. (#1708, fixes
  #848, @rgrinberg)

- Let `Configurator` handle `sizeof` (in addition to negative numbers).
  (#1726, fixes #1723, @Chris00)

- Fix an issue causing menhir generated parsers to fail to build in
  some cases. The fix is to systematically use `-short-paths` when
  calling `ocamlc -i` (#1743, fix #1504, @diml)

- Never raise when printing located errors. The code that would print the
  location excerpts was prone to raising. (#1744, fix #1736, @rgrinberg)

- Add a `dune upgrade` command for upgrading jbuilder projects to Dune
  (#1749, @diml)

- When automatically creating a `dune-project` file, insert the
  detected name in it (#1749, @diml)

- Add `(implicit_transitive_deps <bool>)` mode to dune projects. When this mode
  is turned off, transitive dependencies are not accessible. Only listed
  dependencies are directly accessible. (#1734, #430, @rgrinberg, @hnrgrgr)

- Add `toplevel` stanza. This stanza is used to define toplevels with libraries
  already preloaded. (#1713, @rgrinberg)

- Generate `.merlin` files that account for normal preprocessors defined using a
  subset of the `action` language. (#1768, @rgrinberg)

- Emit `(orig_src_dir <path>)` metadata in `dune-package` for dune packages
  built with `--store-orig-source-dir` command line flag (also controlled by
  `DUNE_STORE_ORIG_SOURCE_DIR` env variable). This is later used to generate
  `.merlin` with `S`-directives pointed to original source locations and thus
  allowing merlin to see those. (#1750, @andreypopp)

- Improve the behavior of `dune promote` when the files to be promoted have been
  deleted. (#1775, fixes #1772, @diml)

- unstable-fmt: preserve comments (#1766, @emillon)

- Pass flags correctly when using `staged_pps` (#1779, fixes #1774, @diml)

- Fix an issue with the use of `(mode promote)` in the menhir
  stanza. It was previously causing intermediate *mock* files to be
  promoted (#1783, fixes #1781, @diml)

- unstable-fmt: ignore files using OCaml syntax (#1784, @emillon)

- Configurator: Add `which` function to replace the `which` command line utility
  in a cross platform way. (#1773, fixes #1705, @Chris00)

- Make configurator append paths to `$PKG_CONFIG_PATH` on macOS. Previously it
  was prepending paths and thus `$PKG_CONFIG_PATH` set by users could have been
  overridden by homebrew installed libraries (#1785, @andreypopp)

- Disallow c/cxx sources that share an object file in the same stubs archive.
  This means that `foo.c` and `foo.cpp` can no longer exist in the same library.
  (#1788, @rgrinberg)

- Forbid use of `%{targets}` (or `${@}` in jbuild files) inside
  preprocessing actions
  (#1812, fixes #1811, @diml)

- Add `DUNE_PROFILE` environment variable to easily set the profile. (#1806,
  @rgrinberg)

- Deprecate the undocumented `(no_keep_locs)` field. It was only
  necessary until virtual libraries were supported (#1822, fix #1816,
  @diml)

- Rename `unstable-fmt` to `format-dune-file` and remove its `--inplace` option.
  (#1821, @emillon).

- Autoformatting: `(using fmt 1.1)` will also format dune files (#1821, @emillon).

- Autoformatting: record dependencies on `.ocamlformat-ignore` files (#1824,
  fixes #1793, @emillon)

1.6.2 (05/12/2018)
------------------

- Fix regression introduced by #1554 reported in:
  https://github.com/ocaml/dune/issues/734#issuecomment-444177134 (#1612,
  @rgrinberg)

- Fix `dune external-lib-deps` when preprocessors are not installed
  (#1607, @diml)

1.6.1 (04/12/2018)
------------------

- Fix hash collision for on-demand ppx rewriters once and for all
  (#1602, fixes #1524, @diml)

- Add `dune external-lib-deps --sexp --unstable-by-dir` so that the output can
  be easily processed by a machine (#1599, @diml)

1.6.0 (29/11/2018)
------------------

- Expand variables in `install` stanzas (#1354, @mseri)

- Add predicate language support for specifying sub directories. This allows the
  use globs, set operations, and special values in specifying the sub
  directories used for the build. For example: `(dirs :standard \ lib*)` will
  use all directories except those that start with `lib`. (#1517, #1568,
  @rgrinberg)

- Add `binaries` field to the `(env ..)` stanza. This field sets and overrides
  binaries for rules defined in a directory. (#1521, @rgrinberg)

- Fix a crash caused by using an extension in a project without
  dune-project file (#1535, fix #1529, @diml)

- Allow `%{bin:..}`, `%{exe:..}`, and other static expansions in the `deps`
  field. (#1155, fix #1531, @rgrinberg)

- Fix bad interaction between on-demand ppx rewriters and using multiple build
  contexts (#1545, @diml)

- Fix handling of installed .dune files when the backend is declared via a
  `dune` file (#1551, fixes #1549, @diml)

- Add a `--stats` command line option to record resource usage (#1543, @diml)

- Fix `dune build @doc` deleting `highlight.pack.js` on rebuilds, after the
  first build (#1557, @aantron).

- Allow targets to be directories, which Dune will treat opaquely
  (#1547, @jordwalke)

- Support for OCaml 4.08: `List.t` is now provided by OCaml (#1561, @ejgallego)

- Exclude the local esy directory (`_esy`) from the list of watched directories
  (#1578, @andreypopp)

- Fix the output of `dune external-lib-deps` (#1594, @diml)

- Introduce `data_only_dirs` to replace `ignored_subdirs`. `ignored_subdirs` is
  deprecated since 1.6. (#1590, @rgrinberg)

1.5.1 (7/11/2018)
-----------------

- Fix `dune utop <dir>` when invoked from a sub-directory of the
  project (#1520, fix #1518, @diml)

- Fix bad interaction between on-demand ppx rewriters and polling mode
  (#1525, fix #1524, @diml)

1.5.0 (1/11/2018)
-----------------

- Filter out empty paths from `OCAMLPATH` and `PATH` (#1436, @rgrinberg)

- Do not add the `lib.cma.js` target in lib's directory. Put this target in a
  sub directory instead. (#1435, fix #1302, @rgrinberg)

- Install generated OCaml files with a `.ml` rather than a `.ml-gen` extension
  (#1425, fix #1414, @rgrinberg)

- Allow to use the `bigarray` library in >= 4.07 without ocamlfind and without
  installing the corresponding `otherlib`. (#1455, @nojb)

- Add `@all` alias to build all targets defined in a directory (#1409, fix
  #1220, @rgrinberg)

- Add `@check` alias to build all targets required for type checking and tooling
  support. (#1447, fix #1220, @rgrinberg)

- Produce the odoc index page with the content wrapper to make it consistent
  with odoc's theming (#1469, @rizo)

- Unblock signals in processes started by dune (#1461, fixes #1451,
  @diml)

- Respect `OCAMLFIND_TOOLCHAIN` and add a `toolchain` option to contexts in the
  workspace file. (#1449, fix #1413, @rgrinberg)

- Fix error message when using `copy_files` stanza to copy files from
  a non sub directory with lang set to dune < 1.3 (#1486, fixes #1485,
  @NathanReb)

- Install man pages in the correct subdirectory (#1483, fixes #1441, @emillon)

- Fix version syntax check for `test` stanza's `action` field. Only
  emits a warning for retro-compatibility (#1474, fixes #1471,
  @NathanReb)

- Interpret the `DESTDIR` environment variable (#1475, @emillon)

- Fix interpretation of paths in `env` stanzas (#1509, fixes #1508, @diml)

- Add `context_name` expansion variable (#1507, @rgrinberg)

- Use shorter paths for generated on-demand ppx drivers. This is to
  help Windows builds where paths are limited in length (#1511, fixes
  #1497, @diml)

- Fix interpretation of `%{env:<var>=<default>}` environment variables
  under `setenv`. Also forbid dynamic environment names or values
  (#1503, @rgrinberg).

1.4.0 (10/10/2018)
------------------

- Do not fail if the output of `ocamlc -config` doesn't include
  `standard_runtime` (#1326, @diml)

- Let `Configurator.V1.C_define.import` handle negative integers
  (#1334, @Chris00)

- Re-execute actions when a target is modified by the user inside
  `_build` (#1343, fix #1342, @diml)

- Pass `--set-switch` to opam (#1341, fix #1337, @diml)

- Fix bad interaction between multi-directory libraries the `menhir`
  stanza (#1373, fix #1372, @diml)

- Integration with automatic formatters (#1252, fix #1201, @emillon)

- Better error message when using `(self_build_stubs_archive ...)` and
  `(c_names ...)` or `(cxx_names ...)` simultaneously.
  (#1375, fix #1306, @nojb)

- Improve name detection for packages when the prefix isn't an actual package
  (#1361, fix #1360, @rgrinberg)

- Support for new menhir rules (#863, fix #305, @fpottier, @rgrinberg)

- Do not remove flags when compiling compatibility modules for wrapped mode
  (#1382, fix #1364, @rgrinberg)

- Fix reason support when using `staged_pps` (#1384, @charlesetc)

- Add support for `enabled_if` in `rule`, `menhir`, `ocamllex`,
  `ocamlyacc` (#1387, @diml)

- Exit gracefully when a signal is received (#1366, @diml)

- Load all defined libraries recursively into utop (#1384, fix #1344,
  @rgrinberg)

- Allow to use libraries `bytes`, `result` and `uchar` without `findlib`
  installed (#1391, @nojb)

- Take argument to self_build_stubs_archive into account. (#1395, @nojb)

- New variable form `%{env:<var>=<default>}` that expands to the environment
  variable `<var>`, or `<default>` if not found. Example: `%{env:BIN=/usr/bin}`.
  (#1305, @trefis)

- Fix bad interaction between `env` customization and vendored
  projects: when a vendored project didn't have its own `env` stanza,
  the `env` stanza from the enclosing project was in effect (#1408,
  @diml)

- Fix stop early bug when scanning for watermarks (#1423, @struktured)

1.3.0 (23/09/2018)
------------------

- Support colors on Windows (#1290, @diml)

- Allow `dune.configurator` and `base` to be used together (#1291, fix
  #1167, @diml)

- Support interrupting and restarting builds on file changes (#1246,
  @kodek16)

- Fix findlib-dynload support with byte mode only (#1295, @bobot)

- Make `dune rules -m` output a valid makefile (#1293, @diml)

- Expand variables in `(targets ..)` field (#1301, #1320, fix #1189, @nojb,
  @rgrinberg, @diml)

- Fix a race condition on Windows that was introduced in 1.2.0
  (#1304, fix #1303, @diml)

- Fix the generation of .merlin files to account for private modules
  (@rgrinberg, fix #1314)

- Exclude the local opam switch directory (`_opam`) from the list of watched
  directories (#1315, @dysinger)

- Fix compilation of the module generated for `findlib.dynload`
  (#1317, fix #1310, @diml)

- Lift restriction on `copy_files` and `copy_files#` stanzas that files to be
  copied should be in a subdirectory of the current directory.
  (#1323, fix #911, @nojb)

1.2.1 (17/09/2018)
------------------

- Enrich the `dune` Emacs mode with syntax highlighting and indentation. New
  file `dune-flymake` to provide a hook `dune-flymake-dune-mode-hook` to enable
  linting of dune files. (#1265, @Chris00)

- Pass `link_flags` to `cc` when compiling with `Configurator.V1.c_test` (#1274,
  @rgrinberg)

- Fix digest calculation of aliases. It should take into account extra bindings
  passed to the alias (#1277, fix #1276, @rgrinberg)

- Fix a bug causing `dune` to fail eagerly when an optional library
  isn't available (#1281, @diml)

- ocamlmklib should use response files only if ocaml >= 4.08 (#1268, @bryphe)

1.2.0 (14/09/2018)
------------------

- Ignore stderr output when trying to find out the number of jobs
  available (#1118, fix #1116, @diml)

- Fix error message when the source directory of `copy_files` does not exist.
  (#1120, fix #1099, @emillon)

- Highlight error locations in error messages (#1121, @emillon)

- Display actual stanza when package is ambiguous (#1126, fix #1123, @emillon)

- Add `dune unstable-fmt` to format `dune` files. The interface and syntax are
  still subject to change, so use with caution. (#1130, fix #940, @emillon)

- Improve error message for `dune utop` without a library name (#1154, fix
  #1149, @emillon)

- Fix parsing `ocamllex` stanza in jbuild files (#1150, @rgrinberg)

- Highlight multi-line errors (#1131, @anuragsoni)

- Do no try to generate shared libraries when this is not supported by
  the OS (#1165, fix #1051, @diml)

- Fix `Flags.write_{sexp,lines}` in configurator by avoiding the use of
  `Stdune.Path` (#1175, fix #1161, @rgrinberg)

- Add support for `findlib.dynload`: when linking an executable using
  `findlib.dynload`, automatically record linked in libraries and
  findlib predicates (#1172, @bobot)

- Add support for promoting a selected list of files (#1192, @diml)

- Add an emacs mode providing helpers to promote correction files
  (#1192, @diml)

- Improve message suggesting to remove parentheses (#1196, fix #1173, @emillon)

- Add `(wrapped (transition "..message.."))` as an option that will generate
  wrapped modules but keep unwrapped modules with a deprecation message to
  preserve compatibility. (#1188, fix #985, @rgrinberg)

- Fix the flags passed to the ppx rewriter when using `staged_pps` (#1218, @diml)

- Add `(env var)` to add a dependency to an environment variable.
  (#1186, @emillon)

- Add a simple version of a polling mode: `dune build -w` keeps
  running and restarts the build when something change on the
  filesystem (#1140, @kodek16)

- Cleanup the way we detect the library search path. We no longer call
  `opam config var lib` in the default build context (#1226, @diml)

- Make test stanzas honor the -p flag. (#1236, fix #1231, @emillon)

- Test stanzas take an optional (action) field to customize how they run (#1248,
  #1195, @emillon)

- Add support for private modules via the `private_modules` field (#1241, fix
  #427, @rgrinberg)

- Add support for passing arguments to the OCaml compiler via a
  response file when the list of arguments is too long (#1256, @diml)

- Do not print diffs by default when running inside dune (#1260, @diml)

- Interpret `$ dune build dir` as building the default alias in `dir`. (#1259,
  @rgrinberg)

- Make the `dynlink` library available without findlib installed (#1270, fix
  #1264, @rgrinberg)

1.1.1 (08/08/2018)
------------------

- Fix `$ jbuilder --dev` (#1104, fixes #1103, @rgrinberg)

- Fix dune exec when `--build-dir` is set to an absolute path (#1105, fixes
  #1101, @rgrinberg)

- Fix duplicate profile argument in suggested command when an external library
  is missing (#1109, #1106, @emillon)

- `-opaque` wasn't correctly being added to modules without an interface.
  (#1108, fix #1107, @rgrinberg)

- Fix validation of library `name` fields and make sure this validation also
  applies when the `name` is derived from the `public_name`. (#1110, fix #1102,
  @rgrinberg)

- Fix a bug causing the toplevel `env` stanza in the workspace file to
  be ignored when at least one context had `(merlin)` (#1114, @diml)

1.1.0 (06/08/2018)
------------------

- Fix lookup of command line specified files when `--root` is given. Previously,
  passing in `--root` in conjunction with `--workspace` or `--config` would not
  work correctly (#997, @rgrinberg)

- Add support for customizing env nodes in workspace files. The `env` stanza is
  now allowed in toplevel position in the workspace file, or for individual
  contexts. This feature requires `(dune lang 1.1)` (#1038, @rgrinberg)

- Add `enabled_if` field for aliases and tests. This field controls whether the
  test will be ran using a boolean expression language. (#819, @rgrinberg)

- Make `name`, `names` fields optional when a `public_name`, `public_names`
  field is provided. (#1041, fix #1000, @rgrinberg)

- Interpret `X` in `--libdir X` as relative to `PREFIX` when `X` is relative
  (#1072, fix #1070, @diml)

- Add support for multi directory libraries by writing
  `(include_subdirs unqualified)` (#1034, @diml)

- Add `(staged_pps ...)` to support staged ppx rewriters such as ones
  using the OCaml typer like `ppx_import` (#1080, fix #193, @diml)

- Use `-opaque` in the `dev` profile. This option trades off binary quality for
  compilation speed when compiling .cmx files. (#1079, fix #1058, @rgrinberg)

- Fix placeholders in `dune subst` documentation (#1090, @emillon, thanks
  @trefis for the bug report)

- Add locations to errors when a missing binary in PATH comes from a dune file
  (#1096, fixes #1095, @rgrinberg)

1.0.1 (19/07/2018)
------------------

- Fix parsing of `%{lib:name:file}` forms (#1022, fixes #1019, @diml)

1.0.0 (10/07/2018)
------------------

- Do not load the user configuration file when running inside dune
  (#700 @diml)

- Do not infer ${null} to be a target (#693 fixes #694 @rgrinberg)

- Introduce jbuilder.configurator library. This is a revived version of
  janestreet's configurator library with better cross compilation support, a
  versioned API, and no external dependencies. (#673, #678 #692, #695
  @rgrinberg)

- Register the transitive dependencies of compilation units as the
  compiler might read `.cm*` files recursively (#666, fixes #660,
  @emillon)

- Fix a bug causing `jbuilder external-lib-deps` to crash (#723,
  @diml)

- `-j` now defaults to the number of processing units available rather
  4 (#726, @diml)

- Fix attaching index.mld to documentation (#731, fixes #717 @rgrinberg)

- Scan the file system lazily (#732, fixes #718 and #228, @diml)

- Add support for setting the default ocaml flags and for build
  profiles (#419, @diml)

- Display a better error messages when writing `(inline_tests)` in an
  executable stanza (#748, @diml)

- Restore promoted files when they are deleted or changed in the
  source tree (#760, fix #759, @diml)

- Fix a crash when using an invalid alias name (#762, fixes #761,
  @diml)

- Fix a crash when using c files from another directory (#758, fixes
  #734, @diml)

- Add an `ignored_subdirs` stanza to replace `jbuild-ignore` files
  (#767, @diml)

- Fix a bug where Dune ignored previous occurrences of duplicated
  fields (#779, @diml)

- Allow setting custom build directories using the `--build-dir` flag or
  `DUNE_BUILD_DIR` environment variable (#846, fix #291, @diml @rgrinberg)

- In dune files, remove support for block (`#| ... |#)`) and sexp
  (`#;`) comments. These were very rarely used and complicate the
  language (#837, @diml)

- In dune files, add support for block strings, allowing to nicely
  format blocks of texts (#837, @diml)

- Remove hard-coded knowledge of ppx_driver and
  ocaml-migrate-parsetree when using a `dune` file (#576, @diml)

- Make the output of Dune slightly more deterministic when run from
  inside Dune (#855, @diml)

- Simplify quoting behavior of variables. All values are now multi-valued and
  whether a multi valued variable is allowed is determined by the quoting and
  substitution context it appears in. (#849, fix #701, @rgrinberg)

- Fix documentation generation for private libraries. (#864, fix #856,
  @rgrinberg)

- Use `Marshal` to store digest and incremental databases. This improves the
  speed of 0 rebuilds. (#817, @diml)

* Allow setting environment variables in `findlib.conf` for cross compilation
  contexts. (#733, @rgrinberg)

- Add a `link_deps` field to executables, to specify link-time dependencies
  like version scripts. (#879, fix #852, @emillon)

- Rename `files_recursively_in` to `source_tree` to make it clearer it
  doesn't include generated files (#899, fix #843, @diml)

- Present the `menhir` stanza as an extension with its own version
  (#901, @diml)

- Improve the syntax of flags in `(pps ...)`. Now instead of `(pps
  (ppx1 -arg1 ppx2 (-foo x)))` one should write `(pps ppx1 -arg ppx2
  -- -foo x)` which looks nicer (#910, @diml)

- Make `(diff a b)` ignore trailing cr on Windows and add `(cmp a b)` for
  comparing binary files (#904, fix #844, @diml)

- Make `dev` the default build profile (#920, @diml)

- Version `dune-workspace` and `~/.config/dune/config` files (#932, @diml)

- Add the ability to build an alias non-recursively from the command
  line by writing `@@alias` (#926, @diml)

- Add a special `default` alias that defaults to `(alias_rec install)`
  when not defined by the user and make `@@default` be the default
  target (#926, @diml)

- Forbid `#require` in `dune` files in OCaml syntax (#938, @diml)

- Add `%{profile}` variable. (#938, @rgrinberg)

- Do not require opam-installer anymore (#941, @diml)

- Add the `lib_root` and `libexec_root` install sections (#947, @diml)

- Rename `path:file` to `dep:file` (#944, @emillon)

- Remove `path-no-dep:file` (#948, @emillon)

- Adapt the behavior of `dune subst` for dune projects (#960, @diml)

- Add the `lib_root` and `libexec_root` sections to install stanzas
  (#947, @diml)

- Add a `Configurator.V1.Flags` module that improves the flag reading/writing
  API (#840, @avsm)

- Add a `tests` stanza that simlpified defining regular and expect tests
  (#822, @rgrinberg)

- Change the `subst` subcommand to lookup the project name from the
  `dune-project` whenever it's available. (#960, @diml)

- The `subst` subcommand no longer looks up the root workspace. Previously this
  detection would break the command whenever `-p` wasn't passed. (#960, @diml)

- Add a `# DUNE_GEN` in META template files. This is done for consistency with
  `# JBUILDER_GEN`. (#958, @rgrinberg)

- Rename the following variables in dune files:
  + `SCOPE_ROOT` to `project_root`
  + `@` to `targets`
  + `^` to `deps`
  `<` was renamed in this PR and latter deleted in favor or named dependencies.
  (#957, @rgrinberg)

- Rename `ROOT` to `workspace_root` in dune files (#993, @diml)

- Lowercase all built-in %{variables} in dune files (#956, @rgrinberg)

- New syntax for naming dependencies: `(deps (:x a b) (:y (glob_files *.c*)))`.
  This replaces the use for `${<}` in dune files. (#950, @diml, @rgrinberg)

- Fix detection of dynamic cycles, which in particular may appear when
  using `(package ..)` dependencies (#988, @diml)

1.0+beta20 (10/04/2018)
-----------------------

- Add a `documentation` stanza. This stanza allows one to attach .mld files to
  opam packages. (#570 @rgrinberg)

- Execute all actions (defined using `(action ..)`) in the context's
  environment. (#623 @rgrinberg)

- Add a `(universe)` special dependency to specify that an action depend on
  everything in the universe. Jbuilder cannot cache the result of an action that
  depend on the universe (#603, fixes #255 @diml)

- Add a `(package <package>)` dependency specification to indicate dependency on
  a whole package. Rules depending on whole package will be executed in an
  environment similar to the one we get once the package is installed (#624,
  @rgrinberg and @diml)

- Don't pass `-runtime-variant _pic` on Windows (#635, fixes #573 @diml)

- Display documentation in alphabetical order. This is relevant to packages,
  libraries, and modules. (#647, fixes #606 @rgrinberg)

- Missing asm in ocaml -config on bytecode only architecture is no longer fatal.
  The same kind of fix is preemptively applied to C compilers being absent.
  (#646, fixes $637 @rgrinberg)

- Use the host's PATH variable when running actions during cross compilation
  (#649, fixes #625 @rgrinberg)

- Fix incorrect include (`-I`) flags being passed to odoc. These flags should be
  directories that include .odoc files, rather than the include flags of the
  libraries. (#652 fixes #651 @rgrinberg)

- Fix a regression introduced by beta19 where the generated merlin
  files didn't include the right `-ppx` flags in some cases (#658
  fixes #657 @diml)

- Fix error message when a public library is defined twice. Before
  jbuilder would raise an uncaught exception (Fixes #661, @diml)

- Fix several cases where `external-lib-deps` was returning too little
  dependencies (#667, fixes #644 @diml)

- Place module list on own line in generated entry point mld (#670 @antron)

- Cosmetic improvements to generated entry point mld (#653 @trefis)

- Remove most useless parentheses from the syntax (#915, @diml)

1.0+beta19.1 (21/03/2018)
-------------------------

- Fix regression introduced by beta19 where duplicate environment variables in
  Unix.environ would cause a fatal error. The first defined environment variable
  is now chosen. (#638 fixed by #640)

- Use ';' as the path separator for OCAMLPATH on Cygwin (#630 fixed by #636
  @diml).

- Use the contents of the `OCAMLPATH` environment variable when not relying on
  `ocamlfind` (#642 @diml)

1.0+beta19 (14/03/2018)
-----------------------

- Ignore errors during the generation of the .merlin (#569, fixes #568 and #51)

- Add a workaround for when a library normally installed by the
  compiler is not installed but still has a META file (#574, fixes
  #563)

- Do not depend on ocamlfind. Instead, hard-code the library path when
  installing from opam (#575)

- Change the default behavior regarding the check for overlaps between
  local and installed libraries. Now even if there is no link time
  conflict, we don't allow an external dependency to overlap with a
  local library, unless the user specifies `allow_overlapping_dependencies`
  in the jbuild file (#587, fixes #562)

- Expose a few more variables in jbuild files: `ext_obj`, `ext_asm`,
  `ext_lib`, `ext_dll` and `ext_exe` as well as `${ocaml-config:XXX}`
  for most variables in the output of `ocamlc -config` (#590)

- Add support for inline and inline expectation tests. The system is
  generic and should support several inline test systems such as
  `ppx_inline_test`, `ppx_expect` or `qtest` (#547)

- Make sure modules in the current directory always have precedence
  over included directories (#597)

- Add support for building executables as object or shared object
  files (#23)

- Add a `best` mode which is native with fallback to byte-code when
  native compilation is not available (#23)

- Fix locations reported in error messages (#609)

- Report error when a public library has a private dependency. Previously, this
  would be silently ignored and install broken artifacts (#607).

- Fix display when output is not a tty (#518)

1.0+beta18.1 (14/03/2018)
-------------------------

- Reduce the number of simultaneously opened fds (#578)

- Always produce an implementation for the alias module, for
  non-jbuilder users (Fix #576)

- Reduce interleaving in the scheduler in an attempt to make Jbuilder
  keep file descriptors open for less long (#586)

- Accept and ignore upcoming new library fields: `ppx.driver`,
  `inline_tests` and `inline_tests.backend` (#588)

- Add a hack to be able to build ppxlib, until beta20 which will have
  generic support for ppx drivers

1.0+beta18 (25/02/2018)
-----------------------

- Fix generation of the implicit alias module with 4.02. With 4.02 it
  must have an implementation while with OCaml >= 4.03 it can be an
  interface only module (#549)

- Let the parser distinguish quoted strings from atoms.  This makes
  possible to use "${v}" to concatenate the list of values provided by
  a split-variable.  Concatenating split-variables with text is also
  now required to be quoted.

- Split calls to ocamldep. Before ocamldep would be called once per
  `library`/`executables` stanza. Now it is called once per file
  (#486)

- Make sure to not pass `-I <stdlib-dir>` to the compiler. It is
  useless and it causes problems in some cases (#488)

- Don't stop on the first error. Before, jbuilder would stop its
  execution after an error was encountered. Now it continues until
  all branches have been explored (#477)

- Add support for a user configuration file (#490)

- Add more display modes and change the default display of
  Jbuilder. The mode can be set from the command line or from the
  configuration file (#490)

- Allow to set the concurrency level (`-j N`) from the configuration file (#491)

- Store artifacts for libraries and executables in separate
  directories. This ensure that Two libraries defined in the same
  directory can't see each other unless one of them depend on the
  other (#472)

- Better support for mli/rei only modules (#489)

- Fix support for byte-code only architectures (#510, fixes #330)

- Fix a regression in `external-lib-deps` introduced in 1.0+beta17
  (#512, fixes #485)

- `@doc` alias will now build only documentation for public libraries. A new
  `@doc-private` alias has been added to build documentation for private
  libraries.

- Refactor internal library management. It should now be possible to
  run `jbuilder build @lint` in Base for instance (#516)

- Fix invalid warning about non-existent directory (#536, fixes #534)

1.0+beta17 (01/02/2018)
-----------------------

- Make jbuilder aware that `num` is an external package in OCaml >= 4.06.0
  (#358)

- `jbuilder exec` will now rebuild the executable before running it if
  necessary. This can be turned off by passing `--no-build` (#345)

- Fix `jbuilder utop` to work in any working directory (#339)

- Fix generation of META synopsis that contains double quotes (#337)

- Add `S .` to .merlin by default (#284)

- Improve `jbuilder exec` to make it possible to execute non public executables.
  `jbuilder exec path/bin` will execute `bin` inside default (or specified)
  context relative to `path`. `jbuilder exec /path` will execute `/path` as
  absolute path but with the context's environment set appropriately. Lastly,
  `jbuilder exec` will change the root as to which paths are relative using the
  `-root` option. (#286)

- Fix `jbuilder rules` printing rules when some binaries are missing (#292)

- Build documentation for non public libraries (#306)

- Fix doc generation when several private libraries have the same name (#369)

- Fix copy# for C/C++ with Microsoft C compiler (#353)

- Add support for cross-compilation. Currently we are supporting the
  opam-cross-x repositories such as
  [opam-cross-windows](https://github.com/whitequark/opam-cross-windows)
  (#355)

- Simplify generated META files: do not generate the transitive
  closure of dependencies in META files (#405)

- Deprecated `${!...}`: the split behavior is now a property of the
  variable. For instance `${CC}`, `${^}`, `${read-lines:...}` all
  expand to lists unless used in the middle of a longer atom (#336)

- Add an `(include ...)` stanza allowing one to include another
  non-generated jbuild file in the current file (#402)

- Add a `(diff <file1> <file2>)` action allowing to diff files and
  promote generated files in case of mismatch (#402, #421)

- Add `jbuilder promote` and `--auto-promote` to promote files (#402,
  #421)

- Report better errors when using `(glob_files ...)` with a directory
  that doesn't exist (#413, Fix #412)

- Jbuilder now properly handles correction files produced by
  ppx_driver. This allows to use `[@@deriving_inline]` in .ml/.mli
  files. This require `ppx_driver >= v0.10.2` to work properly (#415)

- Make jbuilder load rules lazily instead of generating them all
  eagerly. This speeds up the initial startup time of jbuilder on big
  workspaces (#370)

- Now longer generate a `META.pkg.from-jbuilder` file. Now the only
  way to customize the generated `META` file is through
  `META.pkg.template`. This feature was unused and was making the code
  complicated (#370)

- Remove read-only attribute on Windows before unlink (#247)

- Use /Fo instead of -o when invoking the Microsoft C compiler to eliminate
  deprecation warning when compiling C++ sources (#354)

- Add a mode field to `rule` stanzas:
  + `(mode standard)` is the default
  + `(mode fallback)` replaces `(fallback)`
  + `(mode promote)` means that targets are copied to the source tree
  after the rule has completed
  + `(mode promote-until-clean)` is the same as `(mode promote)` except
  that `jbuilder clean` deletes the files copied to the source tree.
  (#437)

- Add a flag `--ignore-promoted-rules` to make jbuilder ignore rules
  with `(mode promote)`. `-p` implies `--ignore-promoted-rules` (#437)

- Display a warning for invalid lines in jbuild-ignore (#389)

- Always build `boot.exe` as a bytecode program. It makes the build of
  jbuilder faster and fix the build on some architectures (#463, fixes #446)

- Fix bad interaction between promotion and incremental builds on OSX
  (#460, fix #456)

- Make the beginning of a new build more explicit in watch mode
  (#2542 @diml)

1.0+beta16 (05/11/2017)
-----------------------

- Fix build on 32-bit OCaml (#313)

1.0+beta15 (04/11/2017)
-----------------------

- Change the semantic of aliases: there are no longer aliases that are
  recursive such as `install` or `runtest`. All aliases are
  non-recursive. However, when requesting an alias from the command
  line, this request the construction of the alias in the specified
  directory and all its children recursively. This allows users to get
  the same behavior as previous recursive aliases for their own
  aliases, such as `example`. Inside jbuild files, one can use `(deps
  (... (alias_rec xxx) ...))` to get the same behavior as on the
  command line. (#268)

- Include sub libraries that have a `.` in the generated documentation index
  (#280).

- Fix "up" links to the top-level index in the odoc generated documentation
  (#282).

- Fix `ARCH_SIXTYFOUR` detection for OCaml 4.06.0 (#303)

1.0+beta14 (11/10/2017)
-----------------------

- Add (copy_files <glob>) and (copy_files# <glob>) stanzas. These
  stanzas setup rules for copying files from a sub-directory to the
  current directory. This provides a reasonable way to support
  multi-directory library/executables in jbuilder (#35, @bobot)

- An empty `jbuild-workspace` file is now interpreted the same as one
  containing just `(context default)`

- Better support for on-demand utop toplevels on Windows and when the
  library has C stubs

- Print `Entering directory '...'` when the workspace root is not the
  current directory. This allows Emacs and Vim to know where relative
  filenames should be interpreted from. Fixes #138

- Fix a bug related to `menhir` stanzas: `menhir` stanzas with a
  `merge_into` field that were in `jbuild` files in sub-directories
  where incorrectly interpreted (#264)

- Add support for locks in actions, for tests that can't be run
  concurrently (#263)

- Support `${..}` syntax in the `include` stanza. (#231)

1.0+beta13 (05/09/2017)
-----------------------

- Generate toplevel html index for documentation (#224, @samoht)

- Fix recompilation of native artifacts. Regression introduced in the last
  version (1.0+beta12) when digests replaces timestamps for checking staleness
  (#238, @dra27)

1.0+beta12 (18/08/2017)
-----------------------

- Fix the quoting of `FLG` lines in generated `.merlin` files (#200,
  @mseri)

- Use the full path of archive files when linking. Before jbuilder
  would do: `-I <path> file.cmxa`, now it does `-I <path>
  <path>/file.cmxa`. Fixes #118 and #177

- Use an absolute path for ppx drivers in `.merlin` files. Merlin
  <3.0.0 used to run ppx commands from the directory where the
  `.merlin` was present but this is no longer the case

- Allow to use `jbuilder install` in contexts other than opam; if
  `ocamlfind` is present in the `PATH` and the user didn't pass
  `--prefix` or `--libdir` explicitly, use the output of `ocamlfind
  printconf destdir` as destination directory for library files (#179,
  @bobot)

- Allow `(:include ...)` forms in all `*flags` fields (#153, @dra27)

- Add a `utop` subcommand. Running `jbuilder utop` in a directory
  builds and executes a custom `utop` toplevel with all libraries
  defined in the current directory (#183, @rgrinberg)

- Do not accept `per_file` anymore in `preprocess` field. `per_file`
  was renamed `per_module` and it is planned to reuse `per_file` for
  another purpose

- Warn when a file is both present in the source tree and generated by
  a rule. Before, jbuilder would silently ignore the rule. One now has
  to add a field `(fallback)` to custom rules to keep the current
  behavior (#218)

- Get rid of the `deprecated-ppx-method` findlib package for ppx
  rewriters (#222, fixes #163)

- Use digests (MD5) of files contents to detect changes rather than
  just looking at the timestamps. We still use timestamps to avoid
  recomputing digests. The performance difference is negligible and we
  avoid more useless recompilations, especially when switching branches
  for instance (#209, fixes #158)

1.0+beta11 (21/07/2017)
-----------------------

- Fix the error message when there are more than one `<package>.opam`
  file for a given package

- Report an error when in a wrapped library, a module that is not the
  toplevel module depends on the toplevel module. This doesn't make as
  such a module would in theory be inaccessible from the outside

- Add `${SCOPE_ROOT}` pointing to the root of the current scope, to
  fix some misuses of `${ROOT}`

- Fix useless hint when all missing dependencies are optional (#137)

- Fix a bug preventing one from generating `META.pkg.template` with a
  custom rule (#190)

- Fix compilation of reason projects: .rei files where ignored and
  caused the build to fail (#184)

1.0+beta10 (08/06/2017)
-----------------------

- Add a `clean` subcommand (@rdavison, #89)

- Add support for generating API documentation with odoc (#74)

- Don't use unix in the bootstrap script, to avoid surprises with
  Cygwin

- Improve the behavior of `jbuilder exec` on Windows

- Add a `--no-buffer` option to see the output of commands in
  real-time. Should only be used with `-j1`

- Deprecate `per_file` in preprocessing specifications and
  rename it `per_module`

- Deprecate `copy-and-add-line-directive` and rename it `copy#`

- Remove the ability to load arbitrary libraries in jbuild file in
  OCaml syntax. Only `unix` is supported since a few released packages
  are using it. The OCaml syntax might eventually be replaced by a
  simpler mechanism that plays better with incremental builds

- Properly define and implement scopes

- Inside user actions, `${^}` now includes files matches by
  `(glob_files ...)` or `(file_recursively_in ...)`

- When the dependencies and targets of a rule can be inferred
  automatically, you no longer need to write them: `(rule (copy a b))`

- Inside `(run ...)`, `${xxx}` forms that expands to lists can now be
  split across multiple arguments by adding a `!`: `${!xxx}`. For
  instance: `(run foo ${!^})`

- Add support for using the contents of a file inside an action:
  - `${read:<file>}`
  - `${read-lines:<file>}`
  - `${read-strings:<file>}` (same as `read-lines` but lines are
    escaped using OCaml convention)

- When exiting prematurely because of a failure, if there are other
  background processes running and they fail, print these failures

- With msvc, `-lfoo` is transparently replaced by `foo.lib` (@dra27, #127)

- Automatically add the `.exe` when installing executables on Windows
  (#123)

- `(run <prog> ...)` now resolves `<prog>` locally if
  possible. i.e. `(run ${bin:prog} ...)` and `(run prog ...)` behave
  the same. This seems like the right default

- Fix a bug where `jbuild rules` would crash instead of reporting a
  proper build error

- Fix a race condition in future.ml causing jbuilder to crash on
  Windows in some cases (#101)

- Fix a bug causing ppx rewriter to not work properly when using
  multiple build contexts (#100)

- Fix .merlin generation: projects in the same workspace are added to
  merlin's source path, so "locate" works on them.

1.0+beta9 (19/05/2017)
----------------------

- Add support for building Reason projects (@rgrinberg, #58)

- Add support for building javascript with js-of-ocaml (@hhugo, #60)

- Better support for topkg release workflow. See
  [topkg-jbuilder](https://github.com/diml/topkg-jbuilder) for more
  details

- Port the manual to rst and setup a jbuilder project on
  readthedocs.org (@rgrinberg, #78)

- Hint for mistyped targets. Only suggest correction on the basename
  for now, otherwise it's slow when the workspace is big

- Add a `(package ...)` field for aliases, so that one can restrict
  tests to a specific package (@rgrinberg, #64)

- Fix a couple of bugs on Windows:
  + fix parsing of end of lines in some cases
  + do not take the case into account when comparing environment
    variable names

- Add AppVeyor CI

- Better error message in case a chain of dependencies *crosses* the
  installed world

- Better error messages for invalid dependency list in jbuild files

- Several improvements/fixes regarding the handling of findlib packages:
  + Better error messages when a findlib package is unavailable
  + Don't crash when an installed findlib package has missing
    dependencies
  + Handle the findlib alternative directory layout which is still
    used by a few packages

- Add `jbuilder installed-libraries --not-available` explaining why
  some libraries are not available

- jbuilder now records dependencies on files of external
  libraries. This mean that when you upgrade a library, jbuilder will
  know what need to be rebuilt.

- Add a `jbuilder rules` subcommand to dump internal compilation
  rules, mostly for debugging purposes

- Ignore all directories starting with a `.` or `_`. This seems to be
  a common pattern:
  - `.git`, `.hg`, `_darcs`
  - `_build`
  - `_opam` (opam 2 local switches)

- Fix the hint for `jbuilder external-lib-deps` (#72)

- Do not require `ocamllex` and `ocamlyacc` to be at the same location
  as `ocamlc` (#75)

1.0+beta8 (17/04/2017)
----------------------

- Added `${lib-available:<library-name>}` which expands to `true` or
  `false` with the same semantic as literals in `(select ...)` stanzas

- Remove hard-coded knowledge of a few specific ppx rewriters to ease
  maintenance moving forward

- Pass the library name to ppx rewriters via the `library-name` cookie

- Fix: make sure the action working directory exist before running it

1.0+beta7 (12/04/2017)
----------------------

- Make the output quieter by default and add a `--verbose` argument
  (@stedolan, #40)

- Various documentation fixes (@adrieng, #41)

- Make `@install` the default target when no targets are specified
  (@stedolan, #47)

- Add predefined support for menhir, similar to ocamlyacc support
  (@rgrinberg, #42)

- Add internal support for sandboxing actions and sandbox the build of
  the alias module with 4.02 to workaround the compiler trying to read
  the cmi of the aliased modules

- Allow to disable dynlink support for libraries via `(no_dynlink)`
  (#55)

- Add a -p/--for-release-of-packages command line argument to simplify
  the jbuilder invocation in opam files and make it more future proof
  (#52)

- Fix the lookup of the executable in `jbuilder exec foo`. Before,
  even if `foo` was to be installed, the freshly built version wasn't
  selected

- Don't generate a `exists_if ...` lines in META files. These are
  useless sine the META files are auto-generated

1.0+beta6 (29/03/2017)
----------------------

- Add an `(executable ...)` stanza for single executables (#33)

- Add a `(package ...)` and `(public_name <name>)/(public_names
   (<names))` to `executable/executables` stanzas to make it easier to
  install executables (#33)

- Fix a bug when using specific rewriters that jbuilder knows about
  without `ppx_driver.runner` (#37). These problem should go away
  soon when we start using `--cookie`

- Fix the interpretation of META files when there is more than one
  applicable assignment. Before this fix, the one with the lowest
  number of formal predicates was selected instead of the one with the
  biggest number of formal predicates

1.0+beta5 (22/03/2017)
----------------------

- When `ocamlfind` is present in the `PATH`, do not attempt to call
  `opam config var lib`

- Make sure the build of jbuilder itself never calls `ocamlfind` or
  `opam`

- Better error message when a jbuild file in OCaml syntax forgets to
  call `Jbuild_plugin.V*.send`

- Added examples of use

- Don't drop inline tests/benchmarks by default

1.0+beta4 (20/03/2017)
----------------------

- Improve error messages about invalid/missing pkg.opam files

- Ignore all errors while running `ocamlfind printconf path`

1.0+beta3 (15/03/2017)
----------------------

- Print optional dependencies as optional in the output of `jbuilder
   external-lib-deps --missing`

- Added a few forms to the DSL:
  - `with-{stderr,outputs}-to`
  - `ignore-{stdout,stderr,outputs}`
- Added `${null}` which expands to `/dev/null` on Unix and `NUL` on
  Windows

- Improve the doc generated by `odoc` for wrapped libraries

- Improve the error reported when an installed package depends on a
  library that is not installed

- Documented `(files_recursively_in ...)`

- Added black box tests

- Fix a bug where `jbuilder` would crash when there was no
  `<package>.opam` file

- Fixed a bug where `.merlin` files where not generated at the root of
  the workspace (#20)

- Fix a bug where a `(glob_files ...)` would cause other dependencies
  to be ignored

- Fix the generated `ppx(...)` line in `META` files

- Fix `(optional)` when a ppx runtime dependency is not available
  (#24)

- Do not crash when an installed package that we don't need has
  missing dependencies (#25)

1.0+beta2 (10/03/2017)
----------------------

- Simplified the rules for finding the root of the workspace as the
  old ones were often picking up the home directory. New rules are:
  + look for a `jbuild-workspace` file in parent directories
  + look for a `jbuild-workspace*` file in parent directories
  + use the current directory
- Fixed the expansion of `${ROOT}` in actions

- Install `quick-start.org` in the documentation directory

- Add a few more things in the log file to help debugging

1.0+beta1 (07/03/2017)
----------------------

- Added a manual

- Support incremental compilation

- Switched the CLI to cmdliner and added a `build` command (#5, @rgrinberg)

- Added a few commands:
  + `runtest`
  + `install`
  + `uninstall`
  + `installed-libraries`
  + `exec`: execute a command in an environment similar to what you
    would get after `jbuilder install`
- Removed the `build-package` command in favor of a `--only-packages`
  option that is common to all commands

- Automatically generate `.merlin` files (#2, @rdavison)

- Improve the output of jbuilder, in particular don't mangle the
  output of commands when using `-j N` with `N > 1`

- Generate a log in `_build/log`

- Versioned the jbuild format and added a first stable version. You
  should now put `(jbuilder_version 1)` in a `jbuild` file at the root
  of your project to ensure forward compatibility

- Switch from `ppx_driver` to `ocaml-migrate-parsetree.driver`. In
  order to use ppx rewriters with Jbuilder, they need to use
  `ocaml-migrate-parsetree.driver`

- Added support for aliases (#7, @rgrinberg)

- Added support for compiling against multiple opam switch
  simultaneously by writing a `jbuild-worspace` file

- Added support for OCaml 4.02.3

- Added support for architectures that don't have natdynlink

- Search the root according to the rules described in the manual
  instead of always using the current directory

- extended the action language to support common actions without using
  a shell:
  + `(with-stdout-to <file> <DSL>)`
  + `(copy <src> <dst>)`
  + ...

- Removed all implicit uses of bash or the system shell. Now one has
  to write explicitly `(bash "...")` or `(system "...")`

- Generate meaningful versions in `META` files

- Strengthen the scope of a package. Jbuilder knows about package
  `foo` only in the sub-tree starting from where `foo.opam` lives

0.1.alpha1 (04/12/2016)
-----------------------

First release
