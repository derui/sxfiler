open Sxfiler_domain
module C = Sxfiler_core
module F = Test_fixtures

let make_left_file_window file_list =
  File_window.make_left ~file_list
    ~history:(Location_history.make ~max_record_num:(Common.Positive_number.make 10 |> Option.get) ())

let make_right_file_window file_list =
  File_window.make_right ~file_list
    ~history:(Location_history.make ~max_record_num:(Common.Positive_number.make 10 |> Option.get) ())

let test_set =
  [
    Alcotest_lwt.test_case_sync "swap file list between sides" `Quick (fun () ->
        let left_list_items = [ F.File_item.fixture (); F.File_item.fixture () ]
        and right_list_items = [ F.File_item.fixture (); F.File_item.fixture () ] in

        let left_list =
          File_list.(
            make ~id:(Id.make "left")
              ~location:(C.Path.of_string "/left" |> Result.get_ok)
              ~sort_order:Types.Sort_type.Name)
        and right_list =
          File_list.(
            make ~id:(Id.make "right")
              ~location:(C.Path.of_string "/right" |> Result.get_ok)
              ~sort_order:Types.Sort_type.Name)
        in

        let left_list = left_list |> File_list.scan (`Scanned left_list_items)
        and right_list = right_list |> File_list.scan (`Scanned right_list_items) in
        let left_file_window = make_left_file_window left_list
        and right_file_window = make_right_file_window right_list in
        let filer = Filer.make ~left_file_window ~right_file_window in
        let Filer.{ left_file_window = left; right_file_window = right } = Filer.swap_side filer in

        Alcotest.(check F.Testable.file_list_scanned) "swapped" left_file_window.file_list right.file_list;
        Alcotest.(check F.Testable.file_list_scanned) "swapped" right_file_window.file_list left.file_list);
  ]
