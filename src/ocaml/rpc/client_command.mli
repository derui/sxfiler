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

  val notify_updated_file_window :
    (G.Filer.UpdatedFileWindowNotificationRequest.t, G.Filer.UpdatedFileWindowNotificationResponse.t) t
  (** command to notify file window updated *)
end

module Completer : sig
  val notify_completed :
    ( G.Completer.Completer.CompletionResultNotificationRequest.t,
      G.Completer.Completer.CompletionResultNotificationResponse.t )
    t
  (** command to notify complete selecting candidates *)
end
