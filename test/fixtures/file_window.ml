open Sxfiler_core
module D = Sxfiler_domain

let fixture_left ?file_list ?history () =
  let file_list = Option.value file_list ~default:(File_list.fixture ()) in
  let history = Option.value history ~default:(D.Location_history.make ()) in
  D.File_window.make_left ~file_list ~history

let fixture_right ?file_list ?history () =
  let file_list = Option.value file_list ~default:(File_list.fixture ()) in
  let history = Option.value history ~default:(D.Location_history.make ()) in
  D.File_window.make_right ~file_list ~history
