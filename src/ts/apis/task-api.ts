import { Reply, ReplyPayload, ReplyKind } from "@/domains/task-reply";

// defines API signature for Task group.

export enum Methods {
  SendReply = "task/sendReply",
  Cancel = "task/cancel",
}

const replyToServerRepresentation = (reply: ReplyPayload): any[] => {
  switch (reply.kind) {
    case ReplyKind.Overwrite:
      return [ReplyKind.Overwrite, true];
    case ReplyKind.Rename:
      return [ReplyKind.Rename, { newName: reply.newName }];
    default:
      throw Error("Unknown reply kind");
  }
};

/**
   API definition for task/sendReply
 */
const SendReply = {
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

/**
   API definition to cancel the task
 */
const Cancel = {
  method: Methods.Cancel,
  parametersTransformer(taskId: string) {
    return taskId;
  },
  resultTransformer() {
    return undefined;
  },
};

export const Apis = { SendReply, Cancel };
