// defines use case to require interaction from a task on the server
import { Actions } from "@/actions";
import { actions } from "@/actions/task";
import { Dispatcher } from "@/types";
import { SyncUseCaseLike } from "@/usecases/type";

export type UseCase = SyncUseCaseLike<Actions, string>;

export const createUseCase = function createUseCase(): UseCase {
  return {
    execute(dispatcher: Dispatcher<Actions>, args) {
      if (!args) {
        throw Error("Need argument");
      }
      dispatcher.dispatch(actions.finished(args));
    },
  };
};
