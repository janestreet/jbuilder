let file = "my-test-files/subdir/test2"
  
let () =
  let oc = open_out file in
    Printf.fprintf oc "bleh";
    close_out oc;