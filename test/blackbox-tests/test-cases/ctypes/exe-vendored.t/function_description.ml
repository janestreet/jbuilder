open Ctypes

module Types = Types_generated

module Functions (F : Ctypes.FOREIGN) = struct
  open F
  let add2 = foreign "example_add2" (int @-> returning int)
end
