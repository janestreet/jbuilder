  $ $JBUILDER build -j1 --root . @install
        ocamlc a/ppx/a.{cmi,cmo,cmt}
        ocamlc a/kernel/a_kernel.{cmi,cmo,cmt}
      ocamlopt a/ppx/a.{cmx,o}
        ocamlc a/ppx/a.cma
      ocamlopt a/kernel/a_kernel.{cmx,o}
        ocamlc a/kernel/a_kernel.cma
      ocamlopt a/ppx/a.{a,cmxa}
      ocamlopt a/kernel/a_kernel.{a,cmxa}
      ocamlopt a/ppx/a.cmxs
      ocamlopt a/kernel/a_kernel.cmxs
      ocamlopt .ppx/a.kernel/ppx.exe
      ocamlopt .ppx/a/ppx.exe
           ppx b/b.pp.ml
      ocamldep b/b.pp.ml.d
        ocamlc b/b.{cmi,cmo,cmt}
      ocamlopt b/b.{cmx,o}
        ocamlc b/b.cma
      ocamlopt b/b.{a,cmxa}
      ocamlopt b/b.cmxs
