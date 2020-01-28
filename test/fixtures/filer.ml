open Sxfiler_core
module D = Sxfiler_domain

let fixture ?left ?right () =
  let left = Option.value left ~default:(File_window.fixture_left ())
  and right = Option.value right ~default:(File_window.fixture_right ()) in
  D.Filer.make ~left_file_window:left ~right_file_window:right
