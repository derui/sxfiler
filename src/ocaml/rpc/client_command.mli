open Abbrev

type ('a, 'b) t = {
  command : G.Service.Command.t;
  writer : 'a -> Pb.Writer.t;
  reader : Pb.Reader.t -> 'b Pb.Result.t;
}
(** type of command called user's side RPC *)

module Filer : sig
  val copy_interaction : (G.Filer.CopyUserDecisionRequest.t, G.Filer.CopyUserDecisionResponse.t) t
  (** command of user interaction for copying *)

  val move_interaction : (G.Filer.MoveUserDecisionRequest.t, G.Filer.MoveUserDecisionResponse.t) t
  (** command of user interaction for moving *)

  val delete_interaction : (G.Filer.DeleteUserDecisionRequest.t, G.Filer.DeleteUserDecisionResponse.t) t
  (** command of user interaction for deletion *)

  val notify_updated : (G.Filer.UpdatedNotificationRequest.t, G.Filer.UpdatedNotificationResponse.t) t
  (** command to notify filer updated *)

  val notify_file_event : (G.Filer.FileEventNotificationRequest.t, G.Filer.FileEventNotificationResponse.t) t
  (** command to notify occurred file events *)
end

module Completer : sig
  val notify_completed :
    ( G.Completer.Completer.CompletionResultNotificationRequest.t,
      G.Completer.Completer.CompletionResultNotificationResponse.t )
    t
  (** command to notify complete selecting candidates *)
end

module Configuration : sig
  val notify_updated : (G.Configuration.UpdatedNotificationRequest.t, G.Configuration.UpdatedNotificationResponse.t) t
  (** command to notify that updating store is complete *)
end
