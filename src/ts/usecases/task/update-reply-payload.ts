// defines use case to require interaction from a task on the server
import { Actions } from "@/actions";
import * as actions from "@/actions/task";
import { Dispatcher } from "@/types";
import { SyncUseCaseLike } from "@/usecases/type";
import { ReplyPayload } from "@/domains/task-reply";

export type UseCase = SyncUseCaseLike<Actions, ReplyPayload>;

export const createUseCase = (): UseCase => {
  return {
    execute(dispatcher: Dispatcher<Actions>, args) {
      if (!args) {
        throw Error("Need argument");
      }
      dispatcher.dispatch(actions.updateReplyPayload(args));
    },
  };
};
