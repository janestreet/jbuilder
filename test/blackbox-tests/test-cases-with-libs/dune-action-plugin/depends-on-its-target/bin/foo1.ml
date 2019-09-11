open Dune_action_plugin

let path = Path.of_string "some_file1"

let action = read_file ~path |> stage ~f:(fun data -> write_file ~path ~data)

let () = run action
