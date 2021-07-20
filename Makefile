.DEFAULT_GOAL := help

PREFIX_ARG := $(if $(PREFIX),--prefix $(PREFIX),)
LIBDIR_ARG := $(if $(LIBDIR),--libdir $(LIBDIR),)
DESTDIR_ARG := $(if $(DESTDIR),--destdir $(DESTDIR),)
INSTALL_ARGS := $(PREFIX_ARG) $(LIBDIR_ARG) $(DESTDIR_ARG)
BIN := ./dune.exe

# Dependencies used for testing dune, when developed locally and
# when tested in CI
TEST_DEPS := \
lwt \
bisect_ppx \
cinaps \
coq-native \
"coq>=8.12.1" \
core_bench \
ctypes \
"csexp>=1.3.0" \
js_of_ocaml \
js_of_ocaml-compiler \
mdx \
menhir \
"merlin>=3.4.0" \
ocamlfind \
ocamlformat.0.18.0 \
"odoc>=1.5.0" \
"ppx_expect>=v0.14" \
ppx_inline_test \
ppxlib \
result \
"utop>=2.6.0"

# Dependencies recommended for developing dune locally,
# but not wanted in CI
DEV_DEPS := \
patdiff

-include Makefile.dev

help:
	@cat doc/make-help.txt

release: $(BIN)
	@$(BIN) build -p dune --profile dune-bootstrap

dune.exe: bootstrap.ml boot/libs.ml boot/duneboot.ml
	@ocaml bootstrap.ml

dev: $(BIN)
	$(BIN) build @install

all: $(BIN)
	$(BIN) build

install:
	$(BIN) install $(INSTALL_ARGS) dune

uninstall:
	$(BIN) uninstall $(INSTALL_ARGS) dune

reinstall: uninstall install

dev-deps:
	opam install -y $(TEST_DEPS)

dev-switch:
	opam update
# Ensuring that either a dev switch already exists or a new one is created
	[[ $(shell opam switch show) == $(shell pwd) ]] || \
		opam switch create -y . 4.11.1 --deps-only --with-test
	opam install -y $(TEST_DEPS) $(DEV_DEPS)

test: $(BIN)
	$(BIN) runtest

test-windows: $(BIN)
	$(BIN) build @runtest-windows

test-js: $(BIN)
	$(BIN) build @runtest-js

test-coq: $(BIN)
	$(BIN) build @runtest-coq

test-all: $(BIN)
	$(BIN) build @runtest @runtest-js @runtest-coq

check: $(BIN)
	$(BIN) build @check

fmt: $(BIN)
	$(BIN) build @fmt --auto-promote

promote: $(BIN)
	$(BIN) promote

accept-corrections: promote

all-supported-ocaml-versions: $(BIN)
	$(BIN) build @install @runtest --workspace dune-workspace.dev --root .

clean: $(BIN)
	$(BIN) clean || true
	rm -rf _boot dune.exe

distclean: clean
	rm -f src/dune_rules/setup.ml

doc:
	sphinx-build doc doc/_build

livedoc:
	cd doc && sphinx-autobuild . _build \
	  -p 8888 -q  --host $(shell hostname) -r '\.#.*'

update-jbuilds: $(BIN)
	$(BIN) build @doc/runtest --auto-promote

# If the first argument is "run"...
ifeq (dune,$(firstword $(MAKECMDGOALS)))
  # use the rest as arguments for "run"
  RUN_ARGS := $(wordlist 2,$(words $(MAKECMDGOALS)),$(MAKECMDGOALS))
  # ...and turn them into do-nothing targets
  $(eval $(RUN_ARGS):;@:)
endif

.PHONY: bench
bench: release
	@dune exec -- ./bench/bench.exe _build/default/dune.exe   2> /dev/null

dune: $(BIN)
	$(BIN) $(RUN_ARGS)

.PHONY: default install uninstall reinstall clean test doc dev-switch
.PHONY: promote accept-corrections release opam-release dune check fmt

# Use this target to make sure that we always run the in source dune when making
# the release
opam-release: dev
	$(BIN) exec -- $(MAKE) dune-release

dune-release:
	dune-release tag
	dune-release distrib --skip-build --skip-lint --skip-tests -n dune
# See https://github.com/ocamllabs/dune-release/issues/206
	DUNE_RELEASE_DELEGATE=github-dune-release-delegate dune-release publish distrib --verbose -n dune
	dune-release opam pkg -n dune
	dune-release opam submit -n dune

# see nix/default.nix for details
.PHONY: nix/opam-selection.nix
nix/opam-selection.nix: Makefile
	nix-shell -A resolve default.nix
