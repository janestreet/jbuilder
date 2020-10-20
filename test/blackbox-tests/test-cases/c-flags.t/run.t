Prior to dune 2.8 ocamlc_cflags and ocamlc_cppflags where
always prepended to the C compiler command line

In the following tests, foo.c is built with the :standard set of flags while
bar.c is built with an empty set of flags.

  $ O_CC=$(ocamlc -config-var c_compiler)
  $ O_CCF=$(ocamlc -config-var ocamlc_cflags)
  $ O_CCPPF=$(ocamlc -config-var ocamlc_cppflags)
  $ O_CC=${O_CC%% }
  $ O_CCF=${O_CCF%% }
  $ O_CCPPF=${O_CCPPF%% }

##########
Dune < 2.8
##########

  $ cat >dune-project <<EOF
  > (lang dune 2.7)

  $ dune rules -m foo.o | tr -s '\t\n\\' ' ' > out_foo
  $ dune rules -m bar.o | sed "s,bar,foo," | tr -s '\t\n\\' ' ' > out_bar

Ocamlc_cflags are duplicated if the :standard set is kept:
  $ cat out_foo | grep -ce "${O_CCF} ${O_CCPPF} ${O_CCF}"
  1

Whether or not the :standard flags is overridden, both ocamlc_cflags and
ocamlc_cpp flags appear in the compiler command line:

  $ cat out_foo | grep -ce "${O_CCF} ${O_CCPPF}"
  1


  $ cat out_bar | grep -ce "${O_CCF} ${O_CCPPF}"
  1

###########
Dune >= 2.8
###########

  $ cat >dune-project <<EOF
  > (lang dune 2.8)

  $ dune rules -m foo.o | tr -s '\t\n\\' ' ' > out_foo
  $ dune rules -m bar.o | sed "s,bar,foo," | tr -s '\t\n\\' ' ' > out_bar

Ocamlc_cflags are not duplicated anymore:
  $ cat out_foo | grep -ce "${O_CCF} ${O_CCPPF} ${O_CCF}"
  0
  [1]

When the :standard flags is overridden, ocamlc_cflags and
ocamlc_cpp are effectively removed from the compiler command line

  $ cat out_foo | grep -ce "${O_CCF} ${O_CCPPF}"
  1

  $ cat out_bar | grep -ce "${O_CCF}"
  0
  [1]

  $ cat out_bar | grep -ce "${O_CCPPF}"
  0
  [1]