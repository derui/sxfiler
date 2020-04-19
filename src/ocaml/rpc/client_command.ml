open Abbrev

type ('a, 'b) t = {
  command : G.Service.Command.t;
  writer : 'a -> Pb.Writer.t;
  reader : Pb.Reader.t -> 'b Pb.Result.t;
}

module Filer = struct
  let copy_interaction =
    {
      command = G.Service.Command.FILER_COPY_INTERACTION;
      writer = G.Filer.CopyUserDecisionRequest.to_proto;
      reader = G.Filer.CopyUserDecisionResponse.from_proto;
    }

  let move_interaction =
    {
      command = G.Service.Command.FILER_MOVE_INTERACTION;
      writer = G.Filer.MoveUserDecisionRequest.to_proto;
      reader = G.Filer.MoveUserDecisionResponse.from_proto;
    }

  let delete_interaction =
    {
      command = G.Service.Command.FILER_DELETE_INTERACTION;
      writer = G.Filer.DeleteUserDecisionRequest.to_proto;
      reader = G.Filer.DeleteUserDecisionResponse.from_proto;
    }

  let notify_updated =
    {
      command = G.Service.Command.FILER_UPDATED;
      writer = G.Filer.UpdatedNotificationRequest.to_proto;
      reader = G.Filer.UpdatedNotificationResponse.from_proto;
    }

  let notify_updated_file_window =
    {
      command = G.Service.Command.FILER_UPDATED_FILE_WINDOW;
      writer = G.Filer.UpdatedFileWindowNotificationRequest.to_proto;
      reader = G.Filer.UpdatedFileWindowNotificationResponse.from_proto;
    }
end

module Completer = struct
  let notify_completed =
    {
      command = G.Service.Command.COMPLETER_NOTIFY_COMPLETED;
      writer = G.Completer.Completer.CompletionResultNotificationRequest.to_proto;
      reader = G.Completer.Completer.CompletionResultNotificationResponse.from_proto;
    }
end
