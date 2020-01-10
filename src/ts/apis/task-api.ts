import { Reply, ReplyKind } from "@/domains/task-reply";
import {
  TaskSendReplyRequest,
  TaskSendReplyResponse,
  TaskReply,
  ReplyType,
  TaskCancelRequest,
  TaskCancelResponse,
} from "../generated/task_pb";
import { Api } from "@/libs/json-rpc/client";

// defines API signature for Task group.

export enum Methods {
  SendReply = "task/sendReply",
  Cancel = "task/cancel",
}

/**
   API definition for task/sendReply
 */
const SendReply: Api<Methods.SendReply, Reply, TaskSendReplyRequest, TaskSendReplyResponse> = {
  method: Methods.SendReply,
  parametersTransformer(param: Reply) {
    const { taskId, reply } = param;
    const reqReply = new TaskReply({
      taskId,
    });

    switch (reply.kind) {
      case ReplyKind.Overwrite:
        reqReply.type = ReplyType.Overwrite;
        reqReply.overwrite = true;
        break;
      case ReplyKind.Rename:
        reqReply.type = ReplyType.Rename;
        const rename = new TaskReply.Rename({
          newName: reply.newName,
        });
        reqReply.rename = rename;
        break;
    }
    return TaskSendReplyRequest.create({ reply: reqReply });
  },
  resultTransformer() {},
};

/**
   API definition to cancel the task
 */
const Cancel: Api<Methods.Cancel, string, TaskCancelRequest, TaskCancelResponse> = {
  method: Methods.Cancel,
  parametersTransformer(taskId: string) {
    const req = TaskCancelRequest.create({ taskId });
    return req;
  },
  resultTransformer() {
    return undefined;
  },
};

export const Apis = { SendReply, Cancel };
