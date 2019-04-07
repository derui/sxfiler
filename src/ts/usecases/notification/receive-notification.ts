import { Actions } from "../../actions";
import { actions } from "../../actions/notification";
import { Notification } from "../../domains/notification";
import { Dispatcher } from "../../types";
import { UseCaseLike } from "../type";

interface Arg {
  notification: Notification;
}

export type UseCase = UseCaseLike<Actions, Arg>;

/**
 * Create the new use case
 */
export const createUseCase = (): UseCase => {
  return {
    execute(dispatcher: Dispatcher<Actions>, arg: Arg) {
      const { notification } = arg;
      dispatcher.dispatch(actions.receiveNotification(notification));
    },
  };
};
