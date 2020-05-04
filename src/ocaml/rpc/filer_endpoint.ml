open Sxfiler_core
(** Filer_op module defines functions for procedures of filer. *)

module S = Ocaml_protoc_plugin.Service
module G = Sxfiler_generated
module F = Sxfiler_workflow
module D = Sxfiler_domain
module E = Endpoint_error

module L = (val Sxfiler_infrastructure.Logger.make [ "endpoint"; "filer" ])

let to_global_events = List.map (fun v -> F.Filer v)

(* endpoint implementations *)

type initialize = F.Filer.Initialize.work_flow -> Endpoint.t

let initialize : initialize =
 fun flow request ->
  Endpoint.with_request (G.Filer.InitializeRequest.from_proto, G.Filer.InitializeResponse.to_proto) request
    ~f:(fun request ->
      let input =
        let open Result.Infix in
        let* left_location =
          Path.of_string request.left_location
          |> Result.map_error (fun _ ->
                 E.invalid_input
                   [ E.Validation_error.make ~field:"left_location" ~message:"Left location must not be empty" ])
        in
        let* right_location =
          Path.of_string request.left_location
          |> Result.map_error (fun _ ->
                 E.invalid_input
                   [ E.Validation_error.make ~field:"right_location" ~message:"Right location must not be empty" ])
        in
        Ok { F.Filer.Initialize.left_location; right_location; left_history = None; right_history = None }
      in
      match input with
      | Error e  -> Lwt.return_error e
      | Ok input ->
          let%lwt events = flow input in
          Lwt.return_ok ((), to_global_events events))

(* reload_all implementation *)
type reload_all = F.Common_step.Filer.get -> F.Filer.Reload_all.work_flow -> Endpoint.t

let reload_all : reload_all =
 fun get_filer flow request ->
  Endpoint.with_request (G.Filer.ReloadAllRequest.from_proto, G.Filer.ReloadAllResponse.to_proto) request ~f:(fun () ->
      let%lwt filer = get_filer () in
      match%lwt flow filer with
      | Error F.Filer.Reload_all.Not_initialized -> E.Filer_error.not_initialized |> E.filer |> Lwt.return_error
      | Ok events -> Lwt.return_ok ((), to_global_events events))

(* move_location implementation *)

type move_location = F.Common_step.Filer.get -> F.Filer.Move_location.work_flow -> Endpoint.t

let move_location : move_location =
 fun get_filer flow request ->
  Endpoint.with_request (G.Filer.MoveLocationRequest.from_proto, G.Filer.MoveLocationResponse.to_proto) request
    ~f:(fun input ->
      let%lwt filer = get_filer () in
      let open Result.Infix in
      let input' =
        let* location = Path.of_string input.location in
        Ok
          {
            F.Filer.Move_location.location;
            side = (match input.side with G.Filer.Side.LEFT -> Left | RIGHT -> Right);
            filer;
          }
      in
      let input' =
        Lwt_result.lift input'
        |> Lwt_result.map_err (fun _ ->
               L.warn (fun m -> m "Invalid location: %s" input.location) |> Lwt.ignore_result;
               E.invalid_input [ E.Validation_error.make ~field:"location" ~message:"location must not be empty" ])
      in
      let open Lwt_result.Infix in
      input' >>= fun input ->
      match%lwt flow input with
      | Error F.Filer.Move_location.Not_initialized -> E.Filer_error.not_initialized |> E.filer |> Lwt.return_error
      | Ok events -> Lwt.return_ok ((), to_global_events events))

(** The function for move_location procedure implementation *)

type open_node = F.Common_step.Filer.get -> F.Filer.Open_node.work_flow -> Endpoint.t

let open_node' get_filer flow input =
  let%lwt filer = get_filer () in
  let item_id = input.G.Filer.OpenFileItemRequest.file_item_id in
  let input' =
    let open Result.Infix in
    let* filer = filer |> Option.to_result ~none:(E.Filer_error.not_initialized |> E.filer) in
    Ok
      {
        F.Filer.Open_node.side = (match input.side with G.Filer.Side.LEFT -> Left | RIGHT -> Right);
        filer;
        item_id = D.File_item.Id.make item_id;
      }
  in
  match input' with
  | Error e  -> Lwt.return_error e
  | Ok input -> (
      match%lwt flow input with
      | Error (F.Filer.Open_node.Item_not_found id) -> E.Filer_error.item_not_found id |> E.filer |> Lwt.return_error
      | Error (F.Filer.Open_node.Location_not_exists path) ->
          E.Filer_error.location_not_exists path |> E.filer |> Lwt.return_error
      | Ok (F.Filer.Open_node.Open_directory events) ->
          let events = to_global_events events in
          Lwt.return_ok ({ G.Filer.OpenFileItemResponse.result = DIRECTORY_OPENED }, events)
      | Ok _ -> Lwt.return_ok ({ G.Filer.OpenFileItemResponse.result = NOT_IMPLEMENTED }, []) )

let open_node : open_node =
 fun get_filer flow request ->
  Endpoint.with_request
    (G.Filer.OpenFileItemRequest.from_proto, G.Filer.OpenFileItemResponse.to_proto)
    request ~f:(open_node' get_filer flow)

(* up_directory implementation *)

type up_directory = F.Common_step.Filer.get -> F.Filer.Up_directory.work_flow -> Endpoint.t

let up_directory : up_directory =
 fun get_filer flow request ->
  Endpoint.with_request (G.Filer.UpDirectoryRequest.from_proto, G.Filer.UpDirectoryResponse.to_proto) request
    ~f:(fun input ->
      let%lwt filer = get_filer () in
      let input =
        { F.Filer.Up_directory.side = (match input.side with G.Filer.Side.LEFT -> Left | RIGHT -> Right); filer }
      in
      match%lwt flow input with
      | Error F.Filer.Up_directory.Not_initialized -> E.Filer_error.not_initialized |> E.filer |> Lwt.return_error
      | Ok events ->
          let res = { G.Filer.UpDirectoryResponse.moved = true } in
          Lwt.return_ok (res, to_global_events events))

(* toggle_mark implementation *)
type toggle_mark = F.Common_step.Filer.get -> F.Filer.Toggle_mark.work_flow -> Endpoint.t

let toggle_mark : toggle_mark =
 fun get_filer flow request ->
  Endpoint.with_request (G.Filer.ToggleMarkOfItemRequest.from_proto, G.Filer.ToggleMarkOfItemResponse.to_proto) request
    ~f:(fun input ->
      let%lwt filer = get_filer () in
      let item_id = input.item_id |> D.File_item.Id.make in
      let input =
        {
          F.Filer.Toggle_mark.side = (match input.side with G.Filer.Side.LEFT -> Left | RIGHT -> Right);
          filer;
          item_id;
        }
      in
      match%lwt flow input with
      | Error F.Filer.Toggle_mark.Not_initialized -> E.Filer_error.not_initialized |> E.filer |> Lwt.return_error
      | Error F.Filer.Toggle_mark.Item_not_found -> E.Filer_error.item_not_found item_id |> E.filer |> Lwt.return_error
      | Ok events -> Lwt.return_ok ((), to_global_events events))

let transfer of_input get_filer flow (input : G.Filer.Transfer.t option) =
  let%lwt filer = get_filer () in
  let input =
    input
    |> Option.to_result
         ~none:([ E.Validation_error.make ~field:"transfer" ~message:"empty transfer" ] |> E.invalid_input)
  in
  let filer = Option.to_result ~none:(E.Filer_error.not_initialized |> E.filer) filer in
  let input =
    let open Result.Infix in
    let* filer = filer in
    let* input = input in
    let target =
      match input.target with
      | G.Filer.Target.MARKED -> F.Filer.Marked
      | ONE                   -> F.Filer.One (D.File_item.Id.make input.target_id)
    in
    let direction =
      match input.direction with
      | G.Filer.Direction.LEFT_TO_RIGHT -> F.Filer.Left_to_right
      | RIGHT_TO_LEFT                   -> Right_to_left
    in
    Ok (of_input filer target direction)
  in
  let open Lwt_result.Infix in
  Lwt_result.lift input >>= fun input ->
  let%lwt events = flow input in
  Lwt.return_ok ((), to_global_events events)

(* move implementation *)
type move = F.Common_step.Filer.get -> F.Filer.Move.work_flow -> Endpoint.t

let move : move =
 fun get_filer flow request ->
  Endpoint.with_request (G.Filer.MoveRequest.from_proto, G.Filer.MoveResponse.to_proto) request ~f:(fun input ->
      let of_input filer target direction = { F.Filer.Move.direction; filer; target } in
      transfer of_input get_filer flow input.transfer)

(* copy implementation *)
type copy = F.Common_step.Filer.get -> F.Filer.Copy.work_flow -> Endpoint.t

let copy : copy =
 fun get_filer flow request ->
  Endpoint.with_request (G.Filer.CopyRequest.from_proto, G.Filer.CopyResponse.to_proto) request ~f:(fun input ->
      let of_input filer target direction = { F.Filer.Copy.direction; filer; target } in
      transfer of_input get_filer flow input.transfer)

(* delete implementation *)
type delete = F.Common_step.Filer.get -> F.Filer.Delete.work_flow -> Endpoint.t

let delete : delete =
 fun get_filer flow request ->
  Endpoint.with_request (G.Filer.DeleteRequest.from_proto, G.Filer.DeleteResponse.to_proto) request ~f:(fun input ->
      let%lwt filer = get_filer () in
      let filer = Option.to_result ~none:(E.Filer_error.not_initialized |> E.filer) filer in
      let input =
        let open Result.Infix in
        let* filer = filer in
        let target =
          match input.target with
          | G.Filer.Target.MARKED -> F.Filer.Marked
          | ONE                   -> F.Filer.One (D.File_item.Id.make input.target_id)
        in
        let side = match input.side with G.Filer.Side.LEFT -> F.Filer.Left | RIGHT -> Right in
        Ok { F.Filer.Delete.side; target; filer }
      in
      let open Lwt_result.Infix in
      Lwt_result.lift input >>= fun input ->
      let%lwt events = flow input in
      Lwt.return_ok ((), to_global_events events))
