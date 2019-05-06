// defines use case to require interaction from a task on the server
import { Actions } from "../../actions";
import { actions } from "../../actions/task";
import { Dispatcher } from "../../types";
import { SyncUseCaseLike } from "../type";
import { Interaction } from "../../domains/task-interaction";

export type UseCase = SyncUseCaseLike<Actions, { interaction: Interaction }>;

export const createUseCase = (): UseCase => {
  return {
    execute(dispatcher: Dispatcher<Actions>, args) {
      if (!args) {
        throw Error("Need argument");
      }
      dispatcher.dispatch(actions.requireInteraction(args.interaction));
    },
  };
};
