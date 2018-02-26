(* Test suite *)
open Mocha_of_ocaml
module R = Reactjscaml

let () =
  Test_c_file_size.suite ();
  Test_c_file_mode.suite ();
  Test_c_file_name.suite ()
