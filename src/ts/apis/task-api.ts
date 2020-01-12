import { Reply, ReplyKind } from "@/domains/task-reply";
import {
  ReplyType,
  TaskCancelRequest,
  TaskCancelResponse,
  TaskReplyToOverwriteRequest,
  TaskReplyToOverwriteResponse,
  TaskReplyToRenameRequest,
  TaskReplyToRenameResponse,
} from "../generated/task_pb";
import { Api } from "@/libs/json-rpc/client";

// defines API signature for Task group.

export enum Methods {
  ReplyToOverwrite = "task/reply/overwrite",
  ReplyToRename = "task/reply/rename",
  Cancel = "task/cancel",
}

/**
   API definition for task/reply/overwrite
 */
const ReplyToOverwrite: Api<
  Methods.ReplyToOverwrite,
  Reply,
  TaskReplyToOverwriteRequest,
  TaskReplyToOverwriteResponse
> = {
  method: Methods.ReplyToOverwrite,
  parametersTransformer(param: Reply) {
    const { taskId, reply } = param;
    const reqReply = new TaskReplyToOverwriteRequest({
      taskId,
    });

    switch (reply.kind) {
      case ReplyKind.Overwrite:
        reqReply.overwrite = true;
        break;
      default:
        throw new Error(`Illegal reply kind given: ${reply.kind}`);
    }
    return reqReply;
  },
  resultTransformer() {},
};

/**
   API definition for task/reply/overwrite
 */
const ReplyToRename: Api<Methods.ReplyToRename, Reply, TaskReplyToRenameRequest, TaskReplyToRenameResponse> = {
  method: Methods.ReplyToRename,
  parametersTransformer(param: Reply) {
    const { taskId, reply } = param;
    const reqReply = new TaskReplyToRenameRequest({
      taskId,
    });

    switch (reply.kind) {
      case ReplyKind.Rename:
        reqReply.newName = reply.newName;
        break;
      default:
        throw new Error(`Illegal reply kind given: ${reply.kind}`);
    }
    return reqReply;
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

export const Apis = { ReplyToOverwrite, ReplyToRename, Cancel };
