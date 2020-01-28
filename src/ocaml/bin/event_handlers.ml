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
  | F.Filer (F.Filer.Updated filer) ->
      let%lwt () = Global.Filer.update (Some filer) in
      let request = { G.Filer.UpdatedNotificationRequest.filer = Tr.Filer.of_domain filer |> Option.some } in
      (Lwt.ignore_result & C.(Client.call instance R.Client_command.Filer.notify_updated request));
      Lwt.return_unit
  | F.Filer (F.Filer.Updated_file_window (side, file_window)) -> (
      let%lwt filer = Global.Filer.get () in
      match filer with
      | None       -> Lwt.return_unit
      | Some filer ->
          let filer =
            match side with
            | F.Filer.Left  -> D.Filer.update_left file_window filer
            | F.Filer.Right -> D.Filer.update_right file_window filer
          in
          let%lwt () = Global.Filer.update (Some filer) in
          let filer' = Tr.Filer.of_domain filer in
          let file_window = match side with Left -> filer'.left_file_window | Right -> filer'.right_file_window in
          let request =
            {
              G.Filer.UpdatedFileWindowNotificationRequest.file_window;
              side = (match side with Left -> G.Filer.Side.LEFT | Right -> G.Filer.Side.RIGHT);
            }
          in
          (Lwt.ignore_result & C.(Client.call instance R.Client_command.Filer.notify_updated_file_window request));
          Lwt.return_unit )
  | _ -> Lwt.return_unit

let setup_handlers (module C : R.Client.Instance) =
  R.Event_handler.setup [ update_filer (module C); notify_candidates (module C) ]
