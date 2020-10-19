open Sxfiler_core
open Sxfiler_domain
include Filer_intf

let file_window_from_side filer side =
  match side with
  | Left  -> filer.Filer.left_file_window |> File_window.as_free
  | Right -> filer.right_file_window |> File_window.as_free

let direction_to_side = function Left_to_right -> (Left, Right) | Right_to_left -> (Right, Left)

let full_path_of_item = function
  | File_item.Marked { full_path; _ }   -> full_path
  | File_item.Unmarked { full_path; _ } -> full_path

let update_side (file_window : File_window.free File_window.t) side filer =
  match side with Left -> Filer.update_left file_window filer | Right -> Filer.update_right file_window filer

(* work flow implementations *)

let initialize get scan_location : Initialize.work_flow =
 fun { left_location; right_location; left_history; right_history; left_sort_order; right_sort_order } ->
  let%lwt filer = get () in
  match filer with
  | Some filer -> Lwt.return [ Updated filer ]
  | None       ->
      let left_list = File_list.make ~id:File_list.Id.(make "left") ~location:left_location ~sort_type:left_sort_order
      and right_list =
        File_list.make ~id:File_list.Id.(make "right") ~location:right_location ~sort_type:right_sort_order
      in
      let scan = Common_step_file_list.scan scan_location in
      let%lwt left_list = scan left_list and right_list = scan right_list in
      let left_history = Option.value ~default:(Location_history.make ()) left_history
      and right_history = Option.value ~default:(Location_history.make ()) right_history in
      let left_file_window = File_window.make_left ~file_list:left_list ~history:left_history
      and right_file_window = File_window.make_right ~file_list:right_list ~history:right_history in
      [ Updated (Filer.make ~left_file_window ~right_file_window) ] |> Lwt.return

let reload_all scan_location : Reload_all.work_flow =
 fun t ->
  (* make dependencies *)
  match t with
  | None   -> Lwt.return_error Reload_all.Not_initialized
  | Some t ->
      let reload = Common_step_file_list.reload in
      let reload_left = Common_step_filer.reload_left scan_location reload
      and reload_right = Common_step_filer.reload_right scan_location reload in
      (* do workflow *)
      let%lwt left_file_window = reload_left t and right_file_window = reload_right t in
      Lwt.return_ok [ Updated (Filer.make ~left_file_window ~right_file_window) ]

let move_location now scan_location : Move_location.work_flow =
 fun { filer; side; location } ->
  (* make dependencies *)
  match filer with
  | None       -> Lwt.return_error Move_location.Not_initialized
  | Some filer ->
      let scan = Common_step_file_list.scan scan_location
      and generate_record = Common_step_location_history.generate_record now in
      let file_window = file_window_from_side filer side in
      let%lwt file_list = File_list.change_location ~location file_window.File_window.file_list |> scan in
      let history = generate_record location |> Fun.flip Location_history.add_record file_window.history in
      let result =
        match side with
        | Left  ->
            let left_file_window = File_window.make_left ~file_list ~history in
            Filer.make ~right_file_window:filer.right_file_window ~left_file_window
        | Right ->
            let right_file_window = File_window.make_right ~file_list ~history in
            Filer.make ~left_file_window:filer.left_file_window ~right_file_window
      in
      Lwt.return_ok [ Updated result ]

(* work flows for manipulation items in filer *)

let copy now demand_action scan_location copy_item : Copy.work_flow =
 fun input ->
  (* setup value and functions *)
  let source_side, dest_side = direction_to_side input.direction in
  let source_file_window = file_window_from_side input.filer source_side in
  let dest_file_window = file_window_from_side input.filer dest_side in
  let targets =
    match input.target with
    | Marked -> File_list.marked_items source_file_window.file_list
    | One id ->
        File_list.find_item ~id source_file_window.file_list |> Option.map (fun v -> [ v ]) |> Option.value ~default:[]
  in
  let dest = File_list.location dest_file_window.file_list in
  let interaction = Common_step_filer.request_copy_interaction demand_action in
  let reload = Common_step_file_list.reload scan_location in

  let rec copy_item' ?(overwrite = false) ?new_name item =
    let item_name = File_item.item item |> File_item.Item.full_path |> Path.basename in
    let dest = Option.value ~default:item_name new_name |> Path.join dest in
    let operation = { Common_step_filer.source = full_path_of_item item; dest; overwrite } in
    let to_result status = { source = full_path_of_item item; dest; status; timestamp = now () } in
    let%lwt result = copy_item operation in
    match result with
    | Ok _ -> to_result Success |> Lwt.return
    | Error (Common_step_filer.Not_exists _) | Error (No_permission _) | Error (Unknown _) ->
        to_result Failed |> Lwt.return
    | Error (Destination_exists _) -> (
        match%lwt interaction item with
        | Error Canceled -> to_result Canceled |> Lwt.return
        | Ok Interaction.Filer_copy_selected.Overwrite -> copy_item' ~overwrite:true item
        | Ok (Interaction.Filer_copy_selected.Rename name) ->
            copy_item' ~new_name:(Common.Not_empty_string.value name) item )
  in

  (* run real workflow *)
  let%lwt results = Lwt_list.map_p copy_item' targets in
  let%lwt dest_file_list = reload dest_file_window.file_list
  and source_file_list = reload source_file_window.file_list in
  let dest_file_window = File_window.reload_list dest_file_list dest_file_window
  and source_file_window = File_window.reload_list source_file_list source_file_window in
  match (dest_file_window, source_file_window) with
  | Ok dest_file_window, Ok source_file_window ->
      let filer = update_side dest_file_window dest_side input.filer |> update_side source_file_window source_side in
      Lwt.return { Copy.events = [ Updated filer ]; results }
  | Error `Not_same, _ | _, Error `Not_same -> Lwt.return { Copy.events = []; results }

let move now demand_action scan_location move_item : Move.work_flow =
 fun input ->
  (* setup value and functions *)
  let source_side, dest_side = direction_to_side input.direction in
  let source_file_window = file_window_from_side input.filer source_side in
  let dest_file_window = file_window_from_side input.filer dest_side in
  let targets =
    match input.target with
    | Marked -> File_list.marked_items source_file_window.file_list
    | One id ->
        File_list.find_item ~id source_file_window.file_list |> Option.map (fun v -> [ v ]) |> Option.value ~default:[]
  in
  let dest = File_list.location dest_file_window.file_list in
  let interaction = Common_step_filer.request_move_interaction demand_action in
  let reload = Common_step_file_list.reload scan_location in

  let rec move_item' ?(overwrite = false) ?new_name item =
    let item_name = full_path_of_item item |> Path.basename in
    let dest = Option.map (Path.join dest) new_name |> Option.value ~default:(Path.join dest item_name) in
    let operation = { Common_step_filer.source = full_path_of_item item; dest; overwrite } in
    let to_result status = { source = full_path_of_item item; dest; status; timestamp = now () } in
    let%lwt result = move_item operation in
    match result with
    | Ok () -> to_result Success |> Lwt.return
    | Error (Common_step_filer.Not_exists _) | Error (No_permission _) | Error (Unknown _) ->
        to_result Failed |> Lwt.return
    | Error (Destination_exists _) -> (
        match%lwt interaction item with
        | Error Canceled -> to_result Canceled |> Lwt.return
        | Ok Interaction.Filer_move_selected.Overwrite -> move_item' ~overwrite:true item
        | Ok (Interaction.Filer_move_selected.Rename name) ->
            move_item' ~new_name:(Common.Not_empty_string.value name) item )
  in

  (* run real workflow *)
  let%lwt results = Lwt_list.map_p move_item' targets in
  let%lwt dest_file_list = reload dest_file_window.file_list
  and source_file_list = reload source_file_window.file_list in
  let dest_file_window = File_window.reload_list dest_file_list dest_file_window
  and source_file_window = File_window.reload_list source_file_list source_file_window in
  match (dest_file_window, source_file_window) with
  | Ok dest_file_window, Ok source_file_window ->
      let filer = input.filer |> update_side dest_file_window dest_side |> update_side source_file_window source_side in
      Lwt.return { Move.events = [ Updated filer ]; results }
  | Error _, _ | _, Error _ -> Lwt.return { Move.events = []; results }

let delete now scan_location delete_item : Delete.work_flow =
 fun input ->
  (* setup value and functions *)
  let file_window = file_window_from_side input.filer input.side in
  let targets =
    match input.target with
    | Marked -> File_list.marked_items file_window.file_list
    | One id -> File_list.find_item ~id file_window.file_list |> Option.map (fun v -> [ v ]) |> Option.value ~default:[]
  in
  let reload = Common_step_file_list.reload scan_location in

  let delete_item' item =
    match%lwt delete_item item with
    | Ok _ -> Lwt.return_some { item; timestamp = now () }
    | Error (Common_step_filer.Not_exists _)
    | Error (No_permission _)
    | Error (Destination_exists _)
    | Error (Unknown _) ->
        Lwt.return_none
  in

  (* run real workflow *)
  let%lwt deleted_items = Lwt_list.filter_map_p delete_item' targets in
  let%lwt file_list = reload file_window.file_list in
  let file_window = File_window.reload_list file_list file_window in
  match file_window with
  | Ok dest_file_window ->
      let filer = update_side dest_file_window input.side input.filer in
      Lwt.return { Delete.events = [ Updated filer ]; results = deleted_items }
  | Error `Not_same     -> Lwt.return { Delete.events = []; results = deleted_items }

(* implementation for open_node flow *)
let open_node scan_location now : Open_node.work_flow =
 fun input ->
  let file_window =
    match input.side with
    | Left  -> input.filer.left_file_window |> D.File_window.as_free
    | Right -> input.filer.right_file_window |> D.File_window.as_free
  in
  let item =
    D.File_list.find_item ~id:input.item_id file_window.file_list
    |> Option.to_result ~none:(`Not_exists input.item_id)
    |> Lwt_result.lift
  in
  let open Lwt_result.Infix in
  let scan_item_location item =
    if D.File_item.is_directory item then
      let location = full_path_of_item item in
      let file_list = D.File_list.change_location ~location file_window.file_list in
      match%lwt Common_step_file_list.scan scan_location file_list with
      | D.File_list.No_location _ -> Lwt.return_error (`Not_exists input.item_id)
      | _ as file_list            -> Lwt.return_ok file_list
    else Lwt.return_error `Do_not_directory
  in
  let move_location_of_file_window file_list =
    let timestamp = now () in
    Lwt_result.lift
    @@ (D.File_window.move_location ~file_list ~timestamp file_window |> Result.map_error (fun _ -> `Same_location))
  in
  let update_filer file_window =
    let filer =
      match input.side with
      | Left  -> Filer.update_left file_window input.filer
      | Right -> Filer.update_right file_window input.filer
    in
    Lwt.return_ok (Open_node.Open_directory [ Updated filer ])
  in
  let%lwt ret = item >>= scan_item_location >>= move_location_of_file_window >>= update_filer in
  match ret with
  | Error (`No_location path) -> Lwt.return_error (Open_node.Location_not_exists path)
  | Error (`Not_exists item) -> Lwt.return_error (Open_node.Item_not_found item)
  (* can not handle same location *)
  | Error `Same_location | Error `Do_not_directory -> Lwt.return_ok Open_node.Not_implemented
  | Ok v -> Lwt.return_ok v

let up_directory scan_location now : Up_directory.work_flow =
 fun { filer; side } ->
  let open Lwt_result in
  Option.to_result ~none:Up_directory.Not_initialized filer |> Lwt_result.lift >>= fun filer ->
  let scan = Common_step_file_list.scan scan_location in
  let file_window =
    match side with
    | Left  -> filer.left_file_window |> File_window.as_free
    | Right -> filer.right_file_window |> File_window.as_free
  in
  let current_location = file_window.File_window.file_list |> File_list.location in
  let parent_location = current_location |> Path.dirname_as_path in
  if Path.equal parent_location current_location then Lwt.return_ok []
  else
    let%lwt file_list = File_list.change_location ~location:parent_location file_window.file_list |> scan in
    let timestamp = now () in
    let file_window = File_window.move_location ~file_list ~timestamp file_window in
    let open Result.Infix in
    let result =
      file_window >>= fun file_window ->
      match side with
      | Left  -> Filer.update_left file_window filer |> Result.ok
      | Right -> Filer.update_right file_window filer |> Result.ok
    in
    match result with Error `Same -> Lwt.return_ok [] | Ok v -> Lwt.return_ok [ Updated v ]

(* implementation for toggle_mark *)
let toggle_mark : Toggle_mark.work_flow =
 fun { filer; side; item_id } ->
  let open Result in
  let mark_item filer =
    let file_window = file_window_from_side filer side in
    match file_window.file_list |> File_list.find_item ~id:item_id with
    | None                        -> Error `Not_found
    | Some (File_item.Marked _)   ->
        let file_list = File_list.unmark_items ~ids:[ item_id ] file_window.file_list in
        Ok (filer, file_list)
    | Some (File_item.Unmarked _) ->
        let file_list = File_list.mark_items ~ids:[ item_id ] file_window.file_list in
        Ok (filer, file_list)
  in
  let update_filer (filer, file_list) =
    let file_window = file_window_from_side filer side in
    let file_window = File_window.reload_list file_list file_window in
    file_window |> Result.map_error (function `Not_same -> `Not_same)
  in

  let result = Option.to_result ~none:`Not_initialized filer >>= mark_item >>= update_filer in
  match result with
  | Error `Not_found | Error `Not_same -> Lwt.return_error Toggle_mark.Item_not_found
  | Error `Not_initialized             -> Lwt.return_error Toggle_mark.Not_initialized
  | Ok file_window                     -> Lwt.return_ok [ Updated_file_window (side, file_window) ]
