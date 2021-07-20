******************************
Dealing with foreign libraries
******************************

The OCaml programming language can interface with libraries written
in foreign languages such as C. This section explains how to do this
with Dune. Note that it does not cover how to write the C stubs
themselves, this is covered by the
`OCaml manual <https://caml.inria.fr/pub/docs/manual-ocaml/intfc.html>`_.

More precisely, this section covers:

- how to add C/C++ stubs to an OCaml library;
- how to pass specific compilation flags for compiling the stubs;
- how to build a library with a foreign build system.

Note that in general Dune has limited support for building source
files written in foreign languages. This support is suitable for most
OCaml projects containing C stubs, but is too limited for building
complex libraries written in C or other languages. For such cases,
Dune can integrate a foreign build system into a normal Dune
build.

Adding C/C++ stubs to an OCaml library
======================================

To add C stubs to an OCaml library, simply list the C files without
the ``.c`` extension in the :ref:`foreign-stubs` field. For instance:

.. code:: scheme

   (library
    (name mylib)
    (foreign_stubs (language c) (names file1 file2)))

You can also add C++ stubs to an OCaml library by specifying
``(language cxx)`` instead.

Dune is currently not flexible regarding the extension of the C/C++
source files. They have to be ``.c`` for C files and ``.cpp``, ``.cc``
or ``.cxx`` for C++ files. If you have source files with other
extensions and you want to build them with Dune, you need to rename
them first. Alternatively, you can use the
:ref:`foreign build sandboxing <foreign-sandboxing>` method described
below.

Header files
------------

C/C++ source files may include header files in the same directory as
the C/C++ source files or in the same directory group when using
:ref:`include_subdirs`.

The header files must have the ``.h`` extension.

Installing header files
-----------------------

It is sometimes desirable to install header files with the
library. For that you have two choices: install them explicitly with
an :ref:`install` stanza or use the ``install_c_headers`` field of the
:ref:`library` stanza. This field takes a list of header files names
without the ``.h`` extension. When a library install header files,
these are made visible to users of the library via the include search
path.

Stub Generation with Ctypes
---------------------------

It is possible to use the ctypes_ stanza to generate bindings for C libraries
without writing any C code.

You need only provide two OCaml modules, named ``Type_description`` and
``Function_description`` which describe the types, values and functions you
want to access in the C library from OCaml.  Additionally, you must list any C
headers and a method for resolving build and link flags.

If you're binding a library distributed by your OS, you can use the
``pkg-config`` utility to resolve any build and link flags.  Alternatively, if
you're using a locally installed library or a vendored library, you can provide
the flags manually.

The ``Type_description`` module must define a functor named ``Types`` with
signature ``Ctypes.TYPE``.  The ``Function_description`` module must define a
functor named ``Functions`` with signature ``Ctypes.FOREIGN``.

.. code:: scheme

   (executable
    (name foo)
    (libraries core)
    (flags (:standard -w -9-27))
    (ctypes
     (external_library_name libfoo)
     (build_flags_resolver pkg_config)
     (headers (include "foo.h"))
     (type_description
      (instance Types)
      (functor Type_description))
     (function_description
      (concurrency unlocked)
      (instance Functions)
      (functor Function_description))
     (generated_entry_point C)))

This stanza will introduce a module named ``C`` into your project, with the
sub-modules ``Types`` and ``Functions`` that will have your fully bound C
types, constants and functions.

A complete example of using the ctypes stanza is available here (TBD).

.. _foreign-sandboxing:

Foreign build sandboxing
========================

When the build of a C library is too complicated to express in the
Dune language, it is possible to simply *sandbox* a foreign
build. Note that this method can be used to build other things, not
just C libraries.

To do that, follow the following procedure:

- put all the foreign code in a sub-directory
- tell Dune not to interpret configuration files in this directory via an
  :ref:`data_only_dirs <dune-data_only_dirs>` stanza
- write a custom rule that:

  - depends on this directory recursively via :ref:`source_tree <source_tree>`
  - invokes the external build system
- *attach* the C archive files to an OCaml library via :ref:`foreign-archives`.

For instance, let's assume that you want to build a C library
``libfoo`` using ``libfoo``'s own build system and attach it to an
OCaml library called ``foo``.

The first step is to put the sources of ``libfoo`` in your project,
for instance in ``src/libfoo``. Then tell dune to consider
``src/libfoo`` as raw data by writing the following in ``src/dune``:

.. code:: scheme

   (data_only_dirs libfoo)

The next step is to setup the rule to build ``libfoo``. For this,
writing the following code ``src/dune``:

.. code:: scheme

   (rule
    (deps (source_tree libfoo))
    (targets libfoo.a dllfoo.so)
    (action
    (no-infer
     (progn
      (chdir libfoo (run make))
      (copy libfoo/libfoo.a libfoo.a)
      (copy libfoo/libfoo.so dllfoo.so)))))

We copy the resulting archive files to the top directory where they can be
declared as ``targets``. The build is done in a ``no-infer`` action because
``libfoo/libfoo.a`` and ``libfoo/libfoo.so`` are dependencies produced by
an external build system.

The last step is to attach these archives to an OCaml library as
follows:

.. code:: scheme

   (library
    (name bar)
    (foreign_archives foo))

Then, whenever you use the ``bar`` library, you will also be able to
use C functions from ``libfoo``.

Limitations
-----------

When using the sandboxing method, the following limitations apply:

- the build of the foreign code will be sequential
- the build of the foreign code won't be incremental

both these points could be improved. If you are interested in helping
make this happen, please let the Dune team know and someone will guide
you.

Real example
------------

The `re2 project <https://github.com/janestreet/re2>`_ uses this
method to build the re2 C library. You can look at the file
``re2/src/re2_c/dune`` in this project to see a full working
example.

.. _ctypes: https://github.com/ocamllabs/ocaml-ctypes
