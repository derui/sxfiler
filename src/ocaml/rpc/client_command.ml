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

  let notify_file_event =
    {
      command = G.Service.Command.FILER_FILE_EVENT;
      writer = G.Filer.FileEventNotificationRequest.to_proto;
      reader = G.Filer.FileEventNotificationResponse.from_proto;
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

module Configuration = struct
  let notify_updated =
    {
      command = G.Service.Command.CONFIGURATION_NOTIFY_UPDATED;
      writer = G.Configuration.UpdatedNotificationRequest.to_proto;
      reader = G.Configuration.UpdatedNotificationResponse.from_proto;
    }
end
