import { Reply, ReplyPayload, ReplyKind } from "../domains/task-reply";

// defines API signature for Task group.

export enum Methods {
  SendReply = "task/sendReply",
}

const replyToServerRepresentation = (reply: ReplyPayload): any[] => {
  switch (reply.kind) {
    case ReplyKind.Overwrite:
      return [ReplyKind.Overwrite];
    case ReplyKind.Rename:
      return [ReplyKind.Rename, { newName: reply.newName }];
    default:
      throw Error("Unknown reply kind");
  }
};

/**
   API definition for keymap/get
 */
const SendInteraction = {
  method: Methods.SendReply,
  parametersTransformer(param: Reply) {
    const { taskId, reply } = param;
    return {
      taskId,
      reply: replyToServerRepresentation(reply),
    };
  },
  resultTransformer() {
    return undefined;
  },
};

export const Apis = { SendInteraction };
