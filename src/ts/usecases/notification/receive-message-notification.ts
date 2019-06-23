import { Actions } from "../../actions";
import { actions } from "../../actions/notification";
import { Dispatcher } from "../../types";
import { UseCaseLike } from "../type";
import { MessageNotification } from "../../domains/message-notification";

interface Arg {
  notification: MessageNotification;
}

export type UseCase = UseCaseLike<Actions, Arg>;

/**
 * Create the new use case
 */
export const createUseCase = (): UseCase => {
  return {
    execute(dispatcher: Dispatcher<Actions>, arg: Arg) {
      const { notification } = arg;
      dispatcher.dispatch(actions.receiveMessage(notification));
    },
  };
};
