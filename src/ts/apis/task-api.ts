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
    const req = new TaskSendReplyRequest();
    const reqReply = new TaskReply();
    reqReply.setTaskid(taskId);
    switch (reply.kind) {
      case ReplyKind.Overwrite:
        reqReply.setType(ReplyType.OVERWRITE);
        reqReply.setOverwrite(true);
        break;
      case ReplyKind.Rename:
        reqReply.setType(ReplyType.RENAME);
        const rename = new TaskReply.Rename();
        rename.setNewname(reply.newName);
        reqReply.setRename(rename);
        break;
    }
    req.setReply(reqReply);
    return req;
  },
  resultTransformer() {},
};

/**
   API definition to cancel the task
 */
const Cancel: Api<Methods.Cancel, string, TaskCancelRequest, TaskCancelResponse> = {
  method: Methods.Cancel,
  parametersTransformer(taskId: string) {
    const req = new TaskCancelRequest();
    req.setTaskid(taskId);
    return req;
  },
  resultTransformer() {
    return undefined;
  },
};

export const Apis = { SendReply, Cancel };
