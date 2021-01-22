open Sxfiler_core
open Sxfiler_domain
module F = Test_fixtures
module S = Sxfiler_workflow.Common_step
module FL = Sxfiler_workflow.Filer

let make_left_file_window file_list = File_window.make_left ~file_list ~history:(Location_history.make ())

let make_right_file_window file_list = File_window.make_right ~file_list ~history:(Location_history.make ())

let sort_items = List.sort & File_item.compare_by Types.Sort_type.Name

let test_set =
  let left_list_items =
    [ F.File_item.fixture (); F.File_item.fixture (); F.File_item.fixture ~stat:(F.File_stat.directory_fixture ()) () ]
  and right_list_items = [ F.File_item.fixture (); F.File_item.fixture () ] in

  let left_list =
    File_list.(
      make ~id:(Id.make "left") ~location:(Path.of_string "/left" |> Result.get_ok) ~sort_type:Types.Sort_type.Name)
  and right_list =
    File_list.(
      make ~id:(Id.make "right") ~location:(Path.of_string "/right" |> Result.get_ok) ~sort_type:Types.Sort_type.Name)
  in
  let filer () =
    let%lwt left_list = left_list |> S.File_list.scan (fun _ -> Lwt.return_ok left_list_items)
    and right_list = right_list |> S.File_list.scan (fun _ -> Lwt.return_ok right_list_items) in
    let left_file_window = make_left_file_window left_list and right_file_window = make_right_file_window right_list in
    Filer.make ~left_file_window ~right_file_window |> Lwt.return
  in

  [
    Alcotest_lwt.test_case "initialize the filer from locations" `Quick (fun _ () ->
        let scan_location path =
          match Path.basename path with
          | "left"  -> Lwt.return_ok left_list_items
          | "right" -> Lwt.return_ok right_list_items
          | _       -> Alcotest.fail "unknown course"
        in
        let get () = Lwt.return_none in
        let work_flow = FL.initialize get scan_location in
        let%lwt filer =
          work_flow
            FL.Initialize.
              {
                left_location = left_list.location;
                right_location = right_list.location;
                left_history = None;
                right_history = None;
                left_sort_order = Types.Sort_type.Name;
                right_sort_order = Types.Sort_type.Name;
              }
        in
        let filer = match filer with [ FL.Initialized v ] -> v | _ -> Alcotest.fail "illegal path" in
        let sort_items = List.sort & File_item.compare_by Types.Sort_type.Name in
        let left_expected = sort_items left_list_items
        and left_actual = File_list.items filer.left_file_window.file_list |> sort_items in
        let right_expected = sort_items right_list_items
        and right_actual = File_list.items filer.right_file_window.file_list |> sort_items in
        Alcotest.(check @@ list F.Testable.file_item) "left initialized" left_expected left_actual;
        Alcotest.(check @@ list F.Testable.file_item) "right initialized" right_expected right_actual;
        Lwt.return_unit);
    Alcotest_lwt.test_case "reload both side one time" `Quick (fun _ () ->
        let work_flow = FL.reload_all (fun _ -> Lwt.return_ok []) in
        let%lwt filer = filer () in
        let%lwt filer' =
          match%lwt work_flow (Some filer) with
          | Ok [ FL.Initialized v ] -> Lwt.return v
          | _                       -> Alcotest.fail "illegal path"
        in
        Alcotest.(check @@ list F.Testable.file_item) "left" [] (File_list.items filer'.left_file_window.file_list);
        Alcotest.(check @@ list F.Testable.file_item) "right" [] (File_list.items filer'.right_file_window.file_list);
        Lwt.return_unit);
    Alcotest_lwt.test_case "delete specified item" `Quick (fun _ () ->
        let work_flow =
          FL.delete
            (fun () -> Time.of_float 0. |> Option.get)
            (fun _ -> Lwt.return_ok [])
            (fun item ->
              Alcotest.(check @@ F.Testable.file_item) "delete" (List.nth left_list_items 0) item;
              Lwt.return_ok ())
        in
        let target = left_list_items |> Fun.flip List.nth 0 in
        let%lwt filer = filer () in
        let%lwt file_window', results =
          match%lwt
            work_flow FL.Delete.{ filer; side = FL.Left; target = One (List.nth left_list_items 0 |> File_item.id) }
          with
          | { events = [ FL.Updated (_, window) ]; results } -> Lwt.return (window, results)
          | _ -> Alcotest.fail "illegal path"
        in
        Alcotest.(check @@ list F.Testable.file_item) "left" [] (File_list.items file_window'.file_list);
        let expected = [ { FL.item = target; timestamp = Time.of_float 0. |> Option.get } ] in
        Alcotest.(check @@ list @@ of_pp FL.pp_delete_result) "deleted" expected results;
        Lwt.return_unit);
    Alcotest_lwt.test_case "copy items" `Quick (fun _ () ->
        let target = List.nth left_list_items 0 in
        let scan_location path =
          match Path.basename path with
          | "left"  -> Lwt.return_ok left_list_items
          | "right" -> Lwt.return_ok (target :: right_list_items)
          | _       -> Alcotest.fail "unknown course"
        in
        let now () = Time.of_float 0. |> Option.get in
        let work_flow =
          FL.copy now
            (fun _ -> Lwt.return Interaction.Canceled)
            scan_location
            (fun { source; _ } ->
              Alcotest.(check & of_pp Path.pp) "source" File_item.(item target |> Item.full_path) source;
              Lwt.return_ok ())
        in
        let%lwt filer = filer () in
        let%lwt (_, right_fw), results =
          match%lwt work_flow FL.Copy.{ filer; direction = FL.Left_to_right; target = One (File_item.id target) } with
          | { events = [ FL.Updated (Left, left); FL.Updated (Right, right) ]; results } ->
              Lwt.return ((left, right), results)
          | _ -> Alcotest.fail "illegal path"
        in
        Alcotest.(check @@ list F.Testable.file_item)
          "right"
          (target :: right_list_items |> sort_items)
          (File_list.items right_fw.file_list |> sort_items);
        Alcotest.(check @@ of_pp Path.pp)
          "source"
          (List.nth results 0 |> fun v -> v.source)
          File_item.(item target |> Item.full_path);
        Lwt.return_unit);
    Alcotest_lwt.test_case "do not copy when it canceled" `Quick (fun _ () ->
        let target = List.nth left_list_items 0 in
        let scan_location path =
          match Path.basename path with
          | "left"  -> Lwt.return_ok left_list_items
          | "right" -> Lwt.return_ok []
          | _       -> Alcotest.fail "unknown course"
        in
        let counter = ref 0 in
        let work_flow =
          FL.copy
            (fun () -> Time.of_float 0. |> Option.get)
            (fun _ -> Lwt.return Interaction.Canceled)
            scan_location
            (fun { source; _ } ->
              if !counter = 0 then (
                incr counter;
                Lwt.return_error (S.Filer.Destination_exists source) )
              else Alcotest.fail "do not call twice")
        in
        let%lwt filer = filer () in
        let%lwt _, right_window', results =
          match%lwt work_flow FL.Copy.{ filer; direction = FL.Left_to_right; target = One (File_item.id target) } with
          | { events = [ FL.Updated (_, left_window); FL.Updated (_, right_window) ]; results } ->
              Lwt.return (left_window, right_window, results)
          | _ -> Alcotest.fail "illegal path"
        in
        Alcotest.(check @@ list F.Testable.file_item) "right" [] (File_list.items right_window'.file_list |> sort_items);
        Alcotest.(check @@ of_pp Path.pp)
          "source"
          (List.nth results 0 |> fun v -> v.source)
          File_item.(item target |> Item.full_path);
        Lwt.return_unit);
    Alcotest_lwt.test_case "move items" `Quick (fun _ () ->
        let target = List.nth left_list_items 0 in
        let left_updated = List.filter (not % File_item.equal target) left_list_items in
        let scan_location path =
          match Path.basename path with
          | "left"  -> Lwt.return_ok left_updated
          | "right" -> Lwt.return_ok (target :: right_list_items)
          | _       -> Alcotest.fail "unknown course"
        in
        let work_flow =
          FL.move
            (fun () -> Time.of_float 0. |> Option.get)
            (fun _ -> Lwt.return Interaction.Canceled)
            scan_location
            (fun { source; dest; _ } ->
              let file_name = File_item.(item target |> Item.full_path) |> Path.basename in
              Alcotest.(check & of_pp Path.pp) "source" File_item.(item target |> Item.full_path) source;
              Alcotest.(check & of_pp Path.pp) "dest" Path.(join right_list.location file_name) dest;
              Lwt.return_ok ())
        in
        let%lwt filer = filer () in
        let%lwt left_window', right_window', results =
          match%lwt work_flow FL.Move.{ filer; direction = FL.Left_to_right; target = One (File_item.id target) } with
          | { events = [ FL.Updated (Left, left_window); FL.Updated (Right, right_window) ]; results } ->
              Lwt.return (left_window, right_window, results)
          | _ -> Alcotest.fail "illegal path"
        in
        Alcotest.(check @@ list F.Testable.file_item)
          "left" (left_updated |> sort_items)
          (File_list.items left_window'.file_list |> sort_items);
        Alcotest.(check @@ list F.Testable.file_item)
          "right"
          (target :: right_list_items |> sort_items)
          (File_list.items right_window'.file_list |> sort_items);
        Alcotest.(check @@ of_pp Path.pp)
          "source"
          (List.nth results 0 |> fun v -> v.source)
          File_item.(item target |> Item.full_path);
        Lwt.return_unit);
    Alcotest_lwt.test_case "open directory node" `Quick (fun _ () ->
        let target = List.nth left_list_items 2 in
        let scan_location _ = Lwt.return_ok right_list_items in
        let work_flow = FL.open_node scan_location (fun () -> Time.of_float 0. |> Option.get) in
        let%lwt filer = filer () in
        let%lwt window' =
          match%lwt work_flow FL.Open_node.{ filer; side = Left; item_id = File_item.id target } with
          | Ok (FL.Open_node.Open_directory [ Location_changed (_, v) ]) -> Lwt.return v
          | _ -> Alcotest.fail "illegal path"
        in
        Alcotest.(check @@ list F.Testable.file_item)
          "filer"
          (window'.File_window.file_list |> File_list.items |> sort_items)
          (right_list_items |> sort_items);
        Alcotest.(check F.Testable.History.t)
          "location"
          (Location_history.make
             ~records:
               [
                 Location_history.Record.make
                   ~location:(File_item.item target |> fun v -> v.File_item.Item.full_path)
                   ~timestamp:(Time.of_float 0. |> Option.get);
               ]
             ())
          window'.File_window.history;
        Lwt.return_unit);
    Alcotest_lwt.test_case "up directory of file list of the side" `Quick (fun _ () ->
        let scan_location path =
          match Path.to_string path with
          | "/" -> Lwt.return_ok right_list_items
          | e   -> Alcotest.fail ("unknown course" ^ e)
        in
        let now () = Time.of_float 1. |> Option.get in
        let work_flow = FL.up_directory scan_location now in
        let%lwt filer = filer () in
        let%lwt ret = work_flow FL.Up_directory.{ side = FL.Left; filer = Some filer } in
        let window = match ret with Ok [ FL.Location_changed (Left, v) ] -> v | _ -> Alcotest.fail "illegal path" in
        let sort_items = List.sort & File_item.compare_by Types.Sort_type.Name in
        let left_expected = sort_items right_list_items
        and left_actual = File_list.items window.file_list |> sort_items in
        let right_expected = sort_items right_list_items
        and right_actual = File_list.items window.file_list |> sort_items in
        Alcotest.(check @@ list F.Testable.file_item) "left initialized" left_expected left_actual;
        Alcotest.(check @@ list F.Testable.file_item) "right initialized" right_expected right_actual;
        Lwt.return_unit);
    Alcotest_lwt.test_case "mark item if it is not marked" `Quick (fun _ () ->
        let work_flow = FL.toggle_mark in
        let%lwt filer = filer () in
        let item_id = List.nth left_list_items 0 |> File_item.id in
        let%lwt ret = work_flow { FL.Toggle_mark.side = FL.Left; filer = Some filer; item_id } in
        let file_window = match ret with Ok [ FL.Updated (_, v) ] -> v | _ -> Alcotest.fail "illegal path" in
        let expected = List.nth left_list_items 0 |> File_item.mark in
        let actual = file_window.file_list |> File_list.find_item ~id:item_id |> Option.get in
        Alcotest.(check @@ F.Testable.file_item) "left item" expected actual;
        Lwt.return_unit);
    Alcotest_lwt.test_case "delete mark from the item if it is marked" `Quick (fun _ () ->
        let work_flow = FL.toggle_mark in
        let%lwt filer = filer () in
        let item_id = List.nth left_list_items 0 |> File_item.id in
        let filer =
          let file_window = filer.Filer.left_file_window |> File_window.as_free in
          let file_list = File_list.mark_items ~ids:[ item_id ] file_window.file_list in
          File_window.reload_list file_list file_window |> Result.get_ok |> Fun.flip Filer.update_left filer
        in
        let%lwt ret = work_flow { FL.Toggle_mark.side = FL.Left; filer = Some filer; item_id } in
        let file_window = match ret with Ok [ FL.Updated (_, v) ] -> v | _ -> Alcotest.fail "illegal path" in
        let expected = List.nth left_list_items 0 in
        let actual = file_window.file_list |> File_list.find_item ~id:item_id |> Option.get in
        Alcotest.(check @@ F.Testable.file_item) "left item" expected actual;
        Lwt.return_unit);
  ]
