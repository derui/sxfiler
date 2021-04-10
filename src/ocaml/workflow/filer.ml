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

(* work flow implementations *)

let initialize
    { Initialize.left_location; right_location; left_history; right_history; left_sort_order; right_sort_order } =
  let open S.Infix in
  let* filer = Common_step_filer.get () in
  match filer with
  | Some filer -> S.return [ Initialized filer ]
  | None       ->
      let left_list = File_list.make ~id:File_list.Id.(make "left") ~location:left_location ~sort_type:left_sort_order
      and right_list =
        File_list.make ~id:File_list.Id.(make "right") ~location:right_location ~sort_type:right_sort_order
      in
      let* left_list = Common_step_file_list.scan left_list in
      let* right_list = Common_step_file_list.scan right_list in
      let left_history = Option.value ~default:(Location_history.make ()) left_history
      and right_history = Option.value ~default:(Location_history.make ()) right_history in
      let left_file_window = File_window.make_left ~file_list:left_list ~history:left_history
      and right_file_window = File_window.make_right ~file_list:right_list ~history:right_history in
      S.return [ Initialized (Filer.make ~left_file_window ~right_file_window) ]

let reload_all () =
  let open S.Infix in
  (* make dependencies *)
  let* filer = Common_step_filer.get () in
  match filer with
  | None   -> S.return (Error Reload_all.Not_initialized)
  | Some t ->
      let* left_file_window = Common_step_filer.reload_left t in
      let* right_file_window = Common_step_filer.reload_right t in
      S.return (Ok [ Initialized (Filer.make ~left_file_window ~right_file_window) ])

let move_location { Move_location.side; location } =
  (* make dependencies *)
  let module C = Common_step_file_list in
  let module L = Common_step_location_history in
  let open S.Infix in
  let* filer = Common_step_filer.get () in
  match filer with
  | Some filer ->
      let file_window = file_window_from_side filer side in
      let* file_list = C.scan @@ File_list.change_location ~location file_window.File_window.file_list in
      let* history = L.generate_record location in
      let history = Location_history.add_record history file_window.history in
      let result =
        match side with
        | Left  -> File_window.make_left ~file_list ~history |> File_window.as_free
        | Right -> File_window.make_right ~file_list ~history |> File_window.as_free
      in
      S.return (Ok [ Location_changed (side, result) ])
  | None       -> S.return (Error Move_location.Not_initialized)

(* work flows for manipulation items in filer *)

let copy input =
  (* setup value and functions *)
  let open S.Infix in
  let* common = S.fetch ~tag:(fun c -> `Step_common_instance c) in
  let* filer_step = S.fetch ~tag:(fun c -> `Step_filer_instance c) in
  let module Common = (val common : Common_step_common.Instance) in
  let module Filer_step = (val filer_step : Common_step_filer.Instance) in
  let* filer = Common_step_filer.get () in
  match filer with
  | None       -> S.return_error Copy.Not_initialized
  | Some filer -> (
      let source_side, dest_side = direction_to_side input.Copy.direction in
      let source_file_window = file_window_from_side filer source_side in
      let dest_file_window = file_window_from_side filer dest_side in
      let target = File_list.find_item ~id:input.target source_file_window.file_list in

      match target with
      | None        -> Lwt.fail_with "Not found file id" |> S.return_lwt
      | Some target -> (
          let dest = File_list.location dest_file_window.file_list in
          let interaction = Common_step_filer.request_copy_interaction in
          let reload = Common_step_file_list.reload in

          let rec copy_item' ?(overwrite = false) ?new_name item =
            let item_name = File_item.item item |> File_item.Item.full_path |> Path.basename in
            let dest = Option.value ~default:item_name new_name |> Path.join dest in
            let operation = { Common_step_filer.source = full_path_of_item item; dest; overwrite } in
            let to_result status = { source = full_path_of_item item; dest; status; timestamp = Common.now () } in
            let* result = Filer_step.copy_item operation |> S.return_lwt in
            match result with
            | Ok _ -> to_result Success |> S.return
            | Error (Common_step_filer.Not_exists _) | Error (No_permission _) | Error (Unknown _) ->
                to_result Failed |> S.return
            | Error (Destination_exists _) -> (
                let* interacted = interaction item in
                match interacted with
                | Error Canceled -> to_result Canceled |> S.return
                | Ok Interaction.Filer_copy_selected.Overwrite -> copy_item' ~overwrite:true item
                | Ok (Interaction.Filer_copy_selected.Rename name) ->
                    copy_item' ~new_name:(D.Common.Not_empty_string.value name) item )
          in

          (* run real workflow *)
          let* result = copy_item' target in
          let* dest_file_list = reload dest_file_window.file_list in
          let* source_file_list = reload source_file_window.file_list in
          let dest_file_window = File_window.reload_list dest_file_list dest_file_window
          and source_file_window = File_window.reload_list source_file_list source_file_window in
          match (dest_file_window, source_file_window) with
          | Ok dest_file_window, Ok source_file_window ->
              S.return_ok
                {
                  Copy.events = [ Updated (source_side, source_file_window); Updated (dest_side, dest_file_window) ];
                  result;
                }
          | Error `Not_same, _ | _, Error `Not_same -> S.return_ok { Copy.events = []; result } ) )

let move input =
  (* setup value and functions *)
  let open S.Infix in
  let* common = S.fetch ~tag:(fun c -> `Step_common_instance c) in
  let* filer_step = S.fetch ~tag:(fun c -> `Step_filer_instance c) in
  let module Common = (val common : Common_step_common.Instance) in
  let module Filer_step = (val filer_step : Common_step_filer.Instance) in
  let* filer = Common_step_filer.get () in
  match filer with
  | None       -> S.return_error Move.Not_initialized
  | Some filer -> (
      let source_side, dest_side = direction_to_side input.Move.direction in
      let source_file_window = file_window_from_side filer source_side in
      let dest_file_window = file_window_from_side filer dest_side in
      let target = File_list.find_item ~id:input.target source_file_window.file_list in
      match target with
      | None        -> Lwt.fail_with "Not found file id" |> S.return_lwt
      | Some target -> (
          let dest = File_list.location dest_file_window.file_list in
          let interaction = Common_step_filer.request_move_interaction in

          let rec move_item' ?(overwrite = false) ?new_name item =
            let item_name = full_path_of_item item |> Path.basename in
            let dest = Option.map (Path.join dest) new_name |> Option.value ~default:(Path.join dest item_name) in
            let operation = { Common_step_filer.source = full_path_of_item item; dest; overwrite } in
            let to_result status = { source = full_path_of_item item; dest; status; timestamp = Common.now () } in
            let* result = Filer_step.move_item operation |> S.return_lwt in
            match result with
            | Ok () -> to_result Success |> S.return
            | Error (Common_step_filer.Not_exists _) | Error (No_permission _) | Error (Unknown _) ->
                to_result Failed |> S.return
            | Error (Destination_exists _) -> (
                let* interacted = interaction item in
                match interacted with
                | Error Canceled -> to_result Canceled |> S.return
                | Ok Interaction.Filer_move_selected.Overwrite -> move_item' ~overwrite:true item
                | Ok (Interaction.Filer_move_selected.Rename name) ->
                    move_item' ~new_name:(D.Common.Not_empty_string.value name) item )
          in

          (* run real workflow *)
          let* result = move_item' target in
          let* dest_file_list = Common_step_file_list.reload dest_file_window.file_list in
          let* source_file_list = Common_step_file_list.reload source_file_window.file_list in
          let dest_file_window = File_window.reload_list dest_file_list dest_file_window
          and source_file_window = File_window.reload_list source_file_list source_file_window in
          match (dest_file_window, source_file_window) with
          | Ok dest_file_window, Ok source_file_window ->
              S.return_ok
                {
                  Move.events = [ Updated (source_side, source_file_window); Updated (dest_side, dest_file_window) ];
                  result;
                }
          | Error _, _ | _, Error _ -> S.return_ok { Move.events = []; result } ) )

let delete input =
  (* setup value and functions *)
  let open S.Infix in
  let* common = S.fetch ~tag:(fun c -> `Step_common_instance c) in
  let* filer_step = S.fetch ~tag:(fun c -> `Step_filer_instance c) in
  let module Common = (val common : Common_step_common.Instance) in
  let module Filer_step = (val filer_step : Common_step_filer.Instance) in
  let* filer = Common_step_filer.get () in
  match filer with
  | None       -> S.return_error Delete.Not_initialized
  | Some filer -> (
      let file_window = file_window_from_side filer input.Delete.side in
      let target = File_list.find_item ~id:input.target file_window.file_list in
      match target with
      | None        -> Lwt.fail_with "Not found file id" |> S.return_lwt
      | Some target -> (
          let reload = Common_step_file_list.reload in

          let delete_item' item =
            let* deleted = Filer_step.delete_item item |> S.return_lwt in
            match deleted with
            | Ok _ -> S.return (Some { item; timestamp = Common.now () })
            | Error (Common_step_filer.Not_exists _)
            | Error (No_permission _)
            | Error (Destination_exists _)
            | Error (Unknown _) ->
                S.return None
          in

          (* run real workflow *)
          let* deleted_item = delete_item' target in
          let* file_list = reload file_window.file_list in
          let file_window = File_window.reload_list file_list file_window in
          match file_window with
          | Ok dest_file_window ->
              S.return_ok { Delete.events = [ Updated (input.side, dest_file_window) ]; result = deleted_item }
          | Error `Not_same     -> S.return_ok { Delete.events = []; result = deleted_item } ) )

(* implementation for open_node flow *)
let open_node input =
  let open S.Infix in
  let* common = S.fetch ~tag:(fun c -> `Step_common_instance c) in
  let module CS = (val common : Common_step_common.Instance) in
  let* filer = Common_step_filer.get () in
  match filer with
  | None       -> S.return_error Open_node.Not_initialized
  | Some filer -> (
      let file_window =
        match input.Open_node.side with
        | Left  -> filer.left_file_window |> D.File_window.as_free
        | Right -> filer.right_file_window |> D.File_window.as_free
      in
      let item =
        D.File_list.find_item ~id:input.item_id file_window.file_list
        |> Option.to_result ~none:(`Not_exists input.item_id)
        |> S.return
      in
      let scan_item_location item =
        match item with
        | Error e -> S.return (Error e)
        | Ok item ->
            if not & D.File_item.is_file item then
              let location = full_path_of_item item in
              let file_list = D.File_list.change_location ~location file_window.file_list in
              let* scanned = Common_step_file_list.scan file_list in
              match scanned with
              | D.File_list.No_location _ -> S.return (Error (`Not_exists input.item_id))
              | _ as file_list            -> S.return (Ok file_list)
            else S.return (Error `Do_not_directory)
      in
      let move_location_of_file_window file_list =
        match file_list with
        | Error e      -> S.return @@ Result.error e
        | Ok file_list ->
            let timestamp = CS.now () in
            S.return
            @@ ( D.File_window.move_location ~file_list ~timestamp file_window
               |> Result.map_error (fun _ -> `Same_location) )
      in
      let update_filer file_window =
        match file_window with
        | Error e        -> S.return_error e
        | Ok file_window -> S.return_ok (Open_node.Open_directory [ Location_changed (input.side, file_window) ])
      in
      let* ret = item >>= scan_item_location >>= move_location_of_file_window >>= update_filer in
      match ret with
      | Error (`No_location path) -> S.return_error (Open_node.Location_not_exists path)
      | Error (`Not_exists item) -> S.return_error (Open_node.Item_not_found item)
      (* can not handle same location *)
      | Error `Same_location | Error `Do_not_directory -> S.return_ok Open_node.Not_implemented
      | Ok v -> S.return_ok v )

let up_directory { Up_directory.side } =
  let open S.Infix in
  let* common = S.fetch ~tag:(fun c -> `Step_common_instance c) in
  let module CS = (val common : Common_step_common.Instance) in
  let* filer = Common_step_filer.get () in
  match filer with
  | None       -> S.return_error Up_directory.Not_initialized
  | Some filer -> (
      let file_window =
        match side with
        | Left  -> filer.left_file_window |> File_window.as_free
        | Right -> filer.right_file_window |> File_window.as_free
      in
      let current_location = file_window.File_window.file_list |> File_list.location in
      let parent_location = current_location |> Path.dirname_as_path in
      if Path.equal parent_location current_location then S.return_ok []
      else
        let* file_list =
          File_list.change_location ~location:parent_location file_window.file_list |> Common_step_file_list.scan
        in
        let timestamp = CS.now () in
        let file_window = File_window.move_location ~file_list ~timestamp file_window in
        match file_window with Error `Same -> S.return_ok [] | Ok v -> S.return_ok [ Location_changed (side, v) ] )

(* implementation for toggle_mark *)
let toggle_mark { Toggle_mark.side; item_id } =
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

  let open S.Infix in
  let* filer = Common_step_filer.get () in
  match filer with
  | None       -> S.return_error Toggle_mark.Not_initialized
  | Some filer -> (
      let open Result.Infix in
      let result = Ok filer >>= mark_item >>= update_filer in
      match result with
      | Error `Not_found | Error `Not_same -> S.return_error Toggle_mark.Item_not_found
      | Ok file_window                     -> S.return_ok [ Updated (side, file_window) ] )
