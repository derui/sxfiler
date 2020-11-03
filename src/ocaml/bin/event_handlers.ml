open Sxfiler_core
module D = Sxfiler_domain
module R = Sxfiler_rpc
module G = Sxfiler_generated
module F = Sxfiler_workflow
module Tr = Sxfiler_translator

let notify_candidates (module C : R.Client.Instance) = function
  | F.Completer (F.Completer.Completed candidates) ->
      let request =
        {
          G.Completer.Completer.CompletionResultNotificationRequest.candidates =
            Tr.Completer.Candidates.of_domain candidates;
        }
      in
      (Lwt.ignore_result & C.(Client.call instance R.Client_command.Completer.notify_completed request));
      Lwt.return_unit
  | _ -> Lwt.return_unit

(* update filer in global state *)
let update_filer (module C : R.Client.Instance) = function
  | F.Filer (F.Filer.Initialized filer) ->
      let%lwt () = Global.Filer.update (Some filer) in
      let request = { G.Filer.UpdatedNotificationRequest.filer = Tr.Filer.of_domain filer |> Option.some } in
      (Lwt.ignore_result & C.(Client.call instance R.Client_command.Filer.notify_updated request));
      Lwt.return_unit
  | F.Filer (F.Filer.Updated (side, file_window')) -> (
      let%lwt filer = Global.Filer.get () in
      match filer with
      | None       -> Lwt.return_unit
      | Some filer ->
          let filer =
            match side with
            | F.Filer.Left  -> D.Filer.update_left file_window' filer
            | F.Filer.Right -> D.Filer.update_right file_window' filer
          in
          let%lwt current_filer = Global.Filer.get () in
          let%lwt () = Global.Filer.update (Some filer) in
          let file_window =
            match (side, current_filer) with
            | Left, Some f  -> f.left_file_window |> D.File_window.as_free |> Option.some
            | Right, Some f -> f.right_file_window |> D.File_window.as_free |> Option.some
            | _, None       -> None
          in
          file_window
          |> Option.iter (fun file_window ->
                 let diff = D.File_list.diff ~prev:file_window.D.File_window.file_list ~next:file_window'.file_list in
                 let request =
                   {
                     G.Filer.FileEventNotificationRequest.file_list_id =
                       D.File_list.id file_window.file_list |> D.File_list.Id.value;
                     events =
                       diff
                       |> List.filter_map (function
                            | `Changed v -> Some v
                            | `Only_left v when F.Filer.equal_side side F.Filer.Left -> Some v
                            | `Only_right v when F.Filer.equal_side side F.Filer.Right -> Some v
                            | _ -> None)
                       |> List.map (fun v ->
                              {
                                G.Filer.FileEvent.event_type = G.Filer.FileEventType.UPDATE;
                                file_item = Tr.File_item.of_domain v |> Option.some;
                              });
                     file_item_orders =
                       (let order = Tr.File_list.of_domain file_window'.file_list in
                        order.G.Filer.FileList.file_item_orders);
                   }
                 in
                 Lwt.ignore_result & C.(Client.call instance R.Client_command.Filer.notify_file_event request));
          Lwt.return_unit )
  | _ -> Lwt.return_unit

let notify_configuration (module C : R.Client.Instance) = function
  | F.Configuration (F.Configuration.Updated store) ->
      let request =
        { G.Configuration.UpdatedNotificationRequest.configurations = Tr.Configuration_store.of_domain store }
      in
      (Lwt.ignore_result & C.(Client.call instance R.Client_command.Configuration.notify_updated request));
      Lwt.return_unit
  | _ -> Lwt.return_unit

let setup_handlers (module C : R.Client.Instance) =
  R.Event_handler.setup [ update_filer (module C); notify_candidates (module C); notify_configuration (module C) ]
