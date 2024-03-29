open Sxfiler_core
(** Filer_op module defines functions for procedures of filer. *)

module Dep = Sxfiler_dependency
module S = Ocaml_protoc_plugin.Service
module G = Sxfiler_generated
module F = Sxfiler_workflow
module D = Sxfiler_domain
module E = Endpoint_error

module L = (val Sxfiler_infrastructure.Logger.make [ "endpoint"; "filer" ])

let to_global_events = List.map (fun v -> F.Filer v)

(* endpoint implementations *)

let initialize workflow deps request =
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
        Ok
          {
            F.Filer.Initialize.left_location;
            right_location;
            left_history = None;
            right_history = None;
            left_sort_order = D.Types.Sort_type.Name;
            right_sort_order = D.Types.Sort_type.Name;
          }
      in
      match input with
      | Error e  -> Lwt.return_error e
      | Ok input ->
          let%lwt events = workflow input |> Dep.provide deps |> Dep.run in
          Lwt.return_ok ((), to_global_events events))

(* reload_all implementation *)
let reload_all workflow deps request =
  Endpoint.with_request (G.Filer.ReloadAllRequest.from_proto, G.Filer.ReloadAllResponse.to_proto) request ~f:(fun () ->
      let%lwt ret = workflow () |> Dep.provide deps |> Dep.run in
      match ret with
      | Error F.Filer.Reload_all.Not_initialized -> E.Filer_error.not_initialized |> E.filer |> Lwt.return_error
      | Ok events -> Lwt.return_ok ((), to_global_events events))

(* move_location implementation *)

let move_location workflow deps request =
  Endpoint.with_request (G.Filer.MoveLocationRequest.from_proto, G.Filer.MoveLocationResponse.to_proto) request
    ~f:(fun input ->
      let open Result.Infix in
      let input' =
        let* location = Path.of_string input.location in
        Ok
          {
            F.Filer.Move_location.location;
            side = (match input.side with G.Filer.Side.LEFT -> Left | RIGHT -> Right);
          }
      in
      let input' =
        Result.map_error
          (fun _ ->
            L.warn (fun m -> m "Invalid location: %s" input.location) |> Lwt.ignore_result;
            E.invalid_input [ E.Validation_error.make ~field:"location" ~message:"location must not be empty" ])
          input'
      in
      match input' with
      | Error _  -> E.Filer_error.not_initialized |> E.filer |> Lwt.return_error
      | Ok input -> (
          let%lwt ret = workflow input |> Dep.provide deps |> Dep.run in
          match ret with
          | Ok events -> Lwt.return_ok ((), to_global_events events)
          | Error F.Filer.Move_location.Not_initialized -> E.Filer_error.not_initialized |> E.filer |> Lwt.return_error))

(** The function for move_location procedure implementation *)

let open_node' workflow deps input =
  let item_id = input.G.Filer.OpenFileItemRequest.file_item_id in
  let input =
    {
      F.Filer.Open_node.side = (match input.side with G.Filer.Side.LEFT -> Left | RIGHT -> Right);
      item_id = D.File_item.Id.make item_id;
    }
  in
  let%lwt ret = workflow input |> Dep.provide deps |> Dep.run in
  match ret with
  | Error F.Filer.Open_node.Not_initialized -> E.Filer_error.not_initialized |> E.filer |> Lwt.return_error
  | Error (F.Filer.Open_node.Item_not_found id) -> E.Filer_error.item_not_found id |> E.filer |> Lwt.return_error
  | Error (F.Filer.Open_node.Location_not_exists path) ->
      E.Filer_error.location_not_exists path |> E.filer |> Lwt.return_error
  | Ok (F.Filer.Open_node.Open_directory events) ->
      let events = to_global_events events in
      Lwt.return_ok ({ G.Filer.OpenFileItemResponse.result = DIRECTORY_OPENED }, events)
  | Ok _ -> Lwt.return_ok ({ G.Filer.OpenFileItemResponse.result = NOT_IMPLEMENTED }, [])

let open_node workflow deps request =
  Endpoint.with_request
    (G.Filer.OpenFileItemRequest.from_proto, G.Filer.OpenFileItemResponse.to_proto)
    request ~f:(open_node' workflow deps)

(* up_directory implementation *)

let up_directory workflow deps request =
  Endpoint.with_request (G.Filer.UpDirectoryRequest.from_proto, G.Filer.UpDirectoryResponse.to_proto) request
    ~f:(fun input ->
      let input =
        { F.Filer.Up_directory.side = (match input.side with G.Filer.Side.LEFT -> Left | RIGHT -> Right) }
      in
      let%lwt ret = workflow input |> Dep.provide deps |> Dep.run in
      match ret with
      | Error F.Filer.Up_directory.Not_initialized -> E.Filer_error.not_initialized |> E.filer |> Lwt.return_error
      | Ok events ->
          let res = { G.Filer.UpDirectoryResponse.moved = true } in
          Lwt.return_ok (res, to_global_events events))

(* toggle_mark implementation *)
let toggle_mark workflow deps request =
  Endpoint.with_request (G.Filer.ToggleMarkOfItemRequest.from_proto, G.Filer.ToggleMarkOfItemResponse.to_proto) request
    ~f:(fun input ->
      let item_id = input.item_id |> D.File_item.Id.make in
      let input =
        { F.Filer.Toggle_mark.side = (match input.side with G.Filer.Side.LEFT -> Left | RIGHT -> Right); item_id }
      in
      let%lwt ret = workflow input |> Dep.provide deps |> Dep.run in
      match ret with
      | Error F.Filer.Toggle_mark.Not_initialized -> E.Filer_error.not_initialized |> E.filer |> Lwt.return_error
      | Error F.Filer.Toggle_mark.Item_not_found -> E.Filer_error.item_not_found item_id |> E.filer |> Lwt.return_error
      | Ok events -> Lwt.return_ok ((), to_global_events events))

let transfer deps of_input to_output flow (input : G.Filer.Transfer.t option) =
  let input =
    input
    |> Option.to_result
         ~none:([ E.Validation_error.make ~field:"transfer" ~message:"empty transfer" ] |> E.invalid_input)
  in
  let input =
    let open Result.Infix in
    let* input = input in
    let target = D.File_item.Id.make input.target_id in
    let direction =
      match input.direction with
      | G.Filer.Direction.LEFT_TO_RIGHT -> F.Filer.Left_to_right
      | RIGHT_TO_LEFT                   -> Right_to_left
    in
    Ok (of_input target direction)
  in
  match input with
  | Error e  -> Lwt.return_error e
  | Ok input ->
      let%lwt output = flow input |> Dep.provide deps |> Dep.run in
      Lwt.return @@ to_output output

(* move implementation *)
let move workflow deps request =
  Endpoint.with_request (G.Filer.MoveRequest.from_proto, G.Filer.MoveResponse.to_proto) request ~f:(fun input ->
      let of_input target direction = { F.Filer.Move.direction; target } in
      let to_output = function
        | Error F.Filer.Move.Not_initialized -> E.Filer_error.not_initialized |> E.filer |> Result.error
        | Ok { F.Filer.Move.events; result } ->
            let result =
              {
                G.Filer.MoveResponse.result =
                  Some
                    ( result |> fun { F.Filer.source; dest; status; timestamp } ->
                      {
                        G.Filer.TransferResult.source = Path.to_string source;
                        destination = Path.to_string dest;
                        status =
                          (match status with
                          | Success  -> G.Filer.TransferStatus.SUCCESS
                          | Failed   -> FAILED
                          | Canceled -> CANCELED);
                        timestamp = Time.to_rfc3339 timestamp;
                      } );
              }
            in
            Ok (result, to_global_events events)
      in
      transfer deps of_input to_output workflow input.transfer)

(* copy implementation *)

let copy workflow deps request =
  Endpoint.with_request (G.Filer.CopyRequest.from_proto, G.Filer.CopyResponse.to_proto) request ~f:(fun input ->
      let of_input target direction = { F.Filer.Copy.direction; target } in
      let to_output = function
        | Error F.Filer.Copy.Not_initialized -> E.Filer_error.not_initialized |> E.filer |> Result.error
        | Ok { F.Filer.Copy.events; result } ->
            let result =
              {
                G.Filer.CopyResponse.result =
                  Some
                    ( result |> fun { F.Filer.source; dest; status; timestamp } ->
                      {
                        G.Filer.TransferResult.source = Path.to_string source;
                        destination = Path.to_string dest;
                        status =
                          (match status with
                          | Success  -> G.Filer.TransferStatus.SUCCESS
                          | Failed   -> FAILED
                          | Canceled -> CANCELED);
                        timestamp = Time.to_rfc3339 timestamp;
                      } );
              }
            in
            Result.ok (result, to_global_events events)
      in
      transfer deps of_input to_output workflow input.transfer)

(* delete implementation *)
let delete workflow deps request =
  Endpoint.with_request (G.Filer.DeleteRequest.from_proto, G.Filer.DeleteResponse.to_proto) request ~f:(fun input ->
      let input =
        let target = D.File_item.Id.make input.target_id in
        let side = match input.side with G.Filer.Side.LEFT -> F.Filer.Left | RIGHT -> Right in
        { F.Filer.Delete.side; target }
      in
      match%lwt workflow input |> Dep.provide deps |> Dep.run with
      | Ok { F.Filer.Delete.events; result } ->
          Lwt.return_ok
            ( {
                G.Filer.DeleteResponse.result =
                  result
                  |> Option.map (fun { F.Filer.item; timestamp } ->
                         let full_path =
                           match item with
                           | D.File_item.Marked { full_path; _ }   -> full_path
                           | D.File_item.Unmarked { full_path; _ } -> full_path
                         in
                         let full_path = Path.to_string full_path in
                         { G.Filer.DeleteResult.path = full_path; timestamp = Time.to_rfc3339 timestamp });
              },
              to_global_events events )
      | Error F.Filer.Delete.Not_initialized -> E.Filer_error.not_initialized |> E.filer |> Lwt.return_error)
