open Sxfiler_core
module D = Sxfiler_domain

let default_history_num = D.Common.Positive_number.make 100 |> Option.get

let fixture_left ?file_list ?history () =
  let file_list = Option.value file_list ~default:(File_list.fixture ()) in
  let history = Option.value history ~default:(D.Location_history.make ~max_record_num:default_history_num ()) in
  D.File_window.make_left ~file_list ~history

let fixture_right ?file_list ?history () =
  let file_list = Option.value file_list ~default:(File_list.fixture ()) in
  let history = Option.value history ~default:(D.Location_history.make ~max_record_num:default_history_num ()) in
  D.File_window.make_right ~file_list ~history
