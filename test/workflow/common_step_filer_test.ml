open Sxfiler_domain
module C = Sxfiler_core
module F = Test_fixtures
module S = Sxfiler_workflow.Common_step

let make_left_file_window file_list =
  File_window.make_left ~file_list
    ~history:(Location_history.make ~max_record_num:(Common.Positive_number.make 10 |> Option.get) ())

let make_right_file_window file_list =
  File_window.make_right ~file_list
    ~history:(Location_history.make ~max_record_num:(Common.Positive_number.make 10 |> Option.get) ())

let test_set =
  let left_list_items = [ F.File_item.fixture (); F.File_item.fixture () ]
  and right_list_items = [ F.File_item.fixture (); F.File_item.fixture () ] in

  let left_list =
    File_list.(
      make ~id:(Id.make "left") ~location:(C.Path.of_string "/left" |> Result.get_ok) ~sort_order:Types.Sort_type.Name)
  and right_list =
    File_list.(
      make ~id:(Id.make "right") ~location:(C.Path.of_string "/right" |> Result.get_ok) ~sort_order:Types.Sort_type.Name)
  in
  let filer () =
    let%lwt left_list = left_list |> S.File_list.scan (fun _ -> Lwt.return_ok left_list_items)
    and right_list = right_list |> S.File_list.scan (fun _ -> Lwt.return_ok right_list_items) in
    let left_file_window = make_left_file_window left_list and right_file_window = make_right_file_window right_list in
    Filer.make ~left_file_window ~right_file_window |> Lwt.return
  in

  [
    Alcotest_lwt.test_case "reload left side file window " `Quick (fun _ () ->
        let%lwt filer = filer () in
        let reload_left = S.Filer.reload_left (fun _ -> Lwt.return_ok []) S.File_list.reload in
        let%lwt left_file_window' = reload_left filer in

        Alcotest.(check @@ list F.Testable.file_item) "swapped" [] (File_list.items left_file_window'.file_list);
        Lwt.return_unit);
    Alcotest_lwt.test_case "reload right side file window " `Quick (fun _ () ->
        let%lwt filer = filer () in
        let reload_right = S.Filer.reload_right (fun _ -> Lwt.return_ok []) S.File_list.reload in
        let%lwt right_file_window' = reload_right filer in

        Alcotest.(check @@ list F.Testable.file_item) "swapped" [] (File_list.items right_file_window'.file_list);
        Lwt.return_unit);
    Alcotest_lwt.test_case "interaction for copy request and response" `Quick (fun _ () ->
        let item = List.nth left_list_items 0 in
        let demand_action = function
          | Interaction.Filer_copy v ->
              Alcotest.(check @@ F.Testable.file_item) "pass item" item v;
              Lwt.return Interaction.(Filer_copy_selected Filer_copy_selected.Overwrite)
          | _                        -> Alcotest.fail "Invalid path"
        in

        let request_copy_interaction = S.Filer.request_copy_interaction demand_action in
        let%lwt event = request_copy_interaction item in

        Alcotest.(check @@ result F.Testable.Interaction.filer_copy_selected @@ of_pp Fmt.nop)
          "swapped" (Ok Interaction.Filer_copy_selected.Overwrite) event;
        Lwt.return_unit);
    Alcotest_lwt.test_case "cancel copy interaction when cancel event given" `Quick (fun _ () ->
        let item = List.nth left_list_items 0 in
        let demand_action = function
          | Interaction.Filer_copy v ->
              Alcotest.(check @@ F.Testable.file_item) "pass item" item v;
              Lwt.return Interaction.(Canceled)
          | _                        -> Alcotest.fail "Invalid path"
        in

        let request_copy_interaction = S.Filer.request_copy_interaction demand_action in
        let%lwt event = request_copy_interaction item in

        Alcotest.(check @@ result F.Testable.Interaction.filer_copy_selected @@ of_pp Fmt.nop)
          "swapped" (Error S.Filer.Canceled) event;
        Lwt.return_unit);
    Alcotest_lwt.test_case "interaction for move request and response" `Quick (fun _ () ->
        let item = List.nth left_list_items 0 in
        let demand_action = function
          | Interaction.Filer_move v ->
              Alcotest.(check @@ F.Testable.file_item) "pass item" item v;
              Lwt.return Interaction.(Filer_move_selected Filer_move_selected.Overwrite)
          | _                        -> Alcotest.fail "Invalid path"
        in

        let request_move_interaction = S.Filer.request_move_interaction demand_action in
        let%lwt event = request_move_interaction item in

        Alcotest.(check @@ result F.Testable.Interaction.filer_move_selected @@ of_pp Fmt.nop)
          "swapped" (Ok Interaction.Filer_move_selected.Overwrite) event;
        Lwt.return_unit);
    Alcotest_lwt.test_case "cancel move interaction when cancel event given" `Quick (fun _ () ->
        let item = List.nth left_list_items 0 in
        let demand_action = function
          | Interaction.Filer_move v ->
              Alcotest.(check @@ F.Testable.file_item) "pass item" item v;
              Lwt.return Interaction.(Canceled)
          | _                        -> Alcotest.fail "Invalid path"
        in

        let request_move_interaction = S.Filer.request_move_interaction demand_action in
        let%lwt event = request_move_interaction item in

        Alcotest.(check @@ result F.Testable.Interaction.filer_move_selected @@ of_pp Fmt.nop)
          "swapped" (Error S.Filer.Canceled) event;
        Lwt.return_unit);
    Alcotest_lwt.test_case "interaction for delete request and response" `Quick (fun _ () ->
        let item = List.nth left_list_items 0 in
        let demand_action = function
          | Interaction.Filer_delete v ->
              Alcotest.(check @@ F.Testable.file_item) "pass item" item v;
              Lwt.return Interaction.(Filer_delete_selected Filer_delete_selected.Confirm)
          | _                          -> Alcotest.fail "Invalid path"
        in

        let request_delete_interaction = S.Filer.request_delete_interaction demand_action in
        let%lwt event = request_delete_interaction item in

        Alcotest.(check @@ result F.Testable.Interaction.filer_delete_selected @@ of_pp Fmt.nop)
          "swapped" (Ok Interaction.Filer_delete_selected.Confirm) event;
        Lwt.return_unit);
    Alcotest_lwt.test_case "cancel delete interaction when cancel event given" `Quick (fun _ () ->
        let item = List.nth left_list_items 0 in
        let demand_action = function
          | Interaction.Filer_delete v ->
              Alcotest.(check @@ F.Testable.file_item) "pass item" item v;
              Lwt.return Interaction.(Canceled)
          | _                          -> Alcotest.fail "Invalid path"
        in

        let request_delete_interaction = S.Filer.request_delete_interaction demand_action in
        let%lwt event = request_delete_interaction item in

        Alcotest.(check @@ result F.Testable.Interaction.filer_delete_selected @@ of_pp Fmt.nop)
          "swapped" (Error S.Filer.Canceled) event;
        Lwt.return_unit);
  ]
