(** {!Notification_reducer} defines handlers for notification sent from server.  *)

module C = Sxfiler_renderer_core

module type Dispatcher = C.Dispatcher_intf.Instance

let expose ~dispatcher:_ server = server
