@all builds private exe's

  $ dune build --display short --root private-exe @all
  Entering directory 'private-exe'
      ocamldep .foo.eobjs/foo.ml.d
        ocamlc .foo.eobjs/byte/foo.{cmi,cmo,cmt}
        ocamlc foo.bc
      ocamlopt .foo.eobjs/native/foo.{cmx,o}
      ocamlopt foo.exe

@all builds private libs

  $ dune build --display short --root private-lib @all
  Entering directory 'private-lib'
      ocamldep .bar.objs/bar.ml.d
        ocamlc .bar.objs/byte/bar.{cmi,cmo,cmt}
      ocamlopt .bar.objs/native/bar.{cmx,o}
        ocamlc bar.cma
      ocamlopt bar.{a,cmxa}
      ocamlopt bar.cmxs

@all builds custom install stanzas

  $ dune build --root install-stanza @subdir/all
  Entering directory 'install-stanza'
  File "default/subdir/_unknown_", line 1, characters 0-0:
  Error: No rule found for subdir/foobar
  May I interest you in one of the following targets instead?
  - default/subdir/dune
  [1]

@all builds user defined rules

  $ dune build --display short --root user-defined @all
  Entering directory 'user-defined'
          echo foo

@all includes user defined install alias

  $ dune build --display short --root install-alias @all
  Entering directory 'install-alias'
          echo foo
