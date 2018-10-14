(** This use case do to notify a message.  *)

module T = Sxfiler_rpc.Types
module C = Sxfiler_renderer_core
module S = Sxfiler_renderer_service

(* The signature declares sleeping in timeout *)
module type Sleeper = sig
  val sleep : float -> unit Lwt.t
end

(* TODO: to be able to inject timeout *)
let timeout = 5.0

module Make (S : Sleeper) = struct
  type param = T.Notification.t
  type t = {param : param}

  let create param = {param}

  let execute t dispatcher =
    let module D = (val dispatcher : C.Dispatcher_intf.Instance) in
    (* do time out if notification is message. *)
    match t.param.body with
    | OneShot _ ->
      D.(Dispatcher.dispatch this C.Message.(Notify_message t.param)) ;
      let id = t.param.id in
      Lwt.(
        S.sleep timeout
        >>= fun () ->
        Lwt.return D.(Dispatcher.dispatch this C.Message.(Timeout_notification_message id)))
    | Progress _ -> Lwt.return D.(Dispatcher.dispatch this C.Message.(Notify_progress t.param))
end
