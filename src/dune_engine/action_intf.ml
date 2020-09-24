open! Stdune

module Outputs = struct
  type t =
    | Stdout
    | Stderr
    | Outputs  (** Both Stdout and Stderr *)
end

module Inputs = struct
  type t = Stdin
end

module Memoize_or_distribute = struct
  type t =
    | Neither
    | Memoize
    | Distribute
end

module Simplified = struct
  type destination =
    | Dev_null
    | File of string

  type source = string

  type t =
    | Run of string * string list
    | Chdir of string
    | Setenv of string * string
    | Redirect_out of t list * Outputs.t * destination
    | Redirect_in of t list * Inputs.t * source
    | Pipe of t list list * Outputs.t
    | Sh of string
end

(** CR-soon cwong: It would be nice to not need to have [Action_ext_intf].
    Currently, we need to strip some dependency stuff from the execution context
    when passing it here, as that forms a dependency cycle ([Deps] -> [Action]
    \-> [Action_intf] -> [Deps]).

    One idea (jdimino's) is to make [ectx] and [eenv] abstract types, which are
    then instantiated with the types in [Action_exec]. I don't like this for a
    bunch of reasons:

    - Using a module type like [module type Ext = sig type ectx ... end] would
      necessitate having a module ascribing to that signature in [Ast], so the
      [Extension] variant can refer to it. Additionally, this means we have to
      rewrite [Action_ast.Make]. The line [include Ast with type t := Ast.t] is
      no longer sound, as there's no good way of proving to the typechecker that
      [Ext.t] and [Ast.Ext.t] are the same type (at least, the compiler didn't
      accept [module Ext = Ast.Ext] when I tried).
    - With the type fully abstracted, there isn't a way to express
      [Action_mapper]. We can work around this by forcing the input and outputs
      to a particular [ext] type, which is safe because we will only ever
      instantiate it to [Action_exec.whatever] anyway. This leads to some nasty
      cycles, however, and I think the easy cycle breaks are quite ugly and
      difficult to read.

    From a performance perspective, I don't think it matters too much -- the
    elements of the record are already allocated, so restricting the tuple only
    allocates the outer record and nothing internally, so fewer than 10 pointers
    total. *)
module Ext = struct
  type ('path, 'target, 'a) t =
    { name : string
    ; version : int
    ; how_to_cache : Memoize_or_distribute.t
    ; (* cwong: I'm not sure how much I like the presence of this field. On the
         one hand, it breaks the intuition that encode/decode are inverses. On
         the other hand, we would only ever encode this type for debugging, so
         it shouldn't matter. *)
      encode : 'a -> Dune_lang.t
    ; simplified : 'a -> Simplified.t list
    ; deps : 'a -> 'path list
    ; targets : 'a -> 'target list
    ; action :
           'a
        -> ectx:Action_ext_intf.context
        -> eenv:Action_ext_intf.env
        -> (* cwong: For now, I think we should only worry about extensions with
              known dependencies. In the future, we may generalize this to
              return an [Action_exec.done_or_more_deps], but that may be
              trickier to get right, and is a bridge we can cross when we get
              there. *)
           unit Fiber.t
    }
end

module type Ast = sig
  type program

  type path

  type target

  type string

  (* This needs to be a type declaration rather than in a module so that
     [include Action_intf.Ast with type t := Ast.t] knows that the [Extension]
     fields are actually the same *)
  type 'a ext = (path, target, 'a) Ext.t

  type t =
    | Run of program * string list
    | With_accepted_exit_codes of int Predicate_lang.t * t
    | Dynamic_run of program * string list
    | Chdir of path * t
    | Setenv of string * string * t
    (* It's not possible to use a build path here since jbuild supports
       redirecting to /dev/null. In [dune] files this is replaced with %{null} *)
    | Redirect_out of Outputs.t * target * t
    | Redirect_in of Inputs.t * path * t
    | Ignore of Outputs.t * t
    | Progn of t list
    | Echo of string list
    | Cat of path
    | Copy of path * target
    | Symlink of path * target
    | Copy_and_add_line_directive of path * target
    | System of string
    | Bash of string
    | Write_file of target * string
    | Rename of target * target
    | Remove_tree of target
    | Mkdir of path
    | Digest_files of path list
    | Diff of (path, target) Diff.t
    | Merge_files_into of path list * string list * target
    | No_infer of t
    | Pipe of Outputs.t * t list
    | Format_dune_file of path * target
    | Cram of path
        (** We encode this variant as such a GADT because:

            - It allows extensions to provide their own state representation
            - By separating that state representation from the functions
              operating on it, we can allocate the operations record once and
              reuse it across multiple instantiations of the same extension,
              instead of allocating a huge record of closures each time *)
    | Extension : 'a * 'a ext -> t
end

module type Helpers = sig
  type program

  type path

  type target

  type string

  type t

  val run : program -> string list -> t

  val chdir : path -> t -> t

  val setenv : string -> string -> t -> t

  val with_stdout_to : target -> t -> t

  val with_stderr_to : target -> t -> t

  val with_outputs_to : target -> t -> t

  val with_stdin_from : path -> t -> t

  val ignore_stdout : t -> t

  val ignore_stderr : t -> t

  val ignore_outputs : t -> t

  val progn : t list -> t

  val echo : string list -> t

  val cat : path -> t

  val copy : path -> target -> t

  val symlink : path -> target -> t

  val copy_and_add_line_directive : path -> target -> t

  val system : string -> t

  val bash : string -> t

  val write_file : target -> string -> t

  val rename : target -> target -> t

  val remove_tree : target -> t

  val mkdir : path -> t

  val digest_files : path list -> t

  val diff : ?optional:bool -> ?mode:Diff.Mode.t -> path -> target -> t

  val format_dune_file : path -> target -> t
end
