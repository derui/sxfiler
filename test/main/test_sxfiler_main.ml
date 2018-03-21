module M = Sxfiler_main

let () =
  M.File_list.Test.suite ();
  M.User_data.Test.suite ();
