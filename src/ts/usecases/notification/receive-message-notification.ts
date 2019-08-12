import { Actions } from "@/actions";
import { actions } from "@/actions/notification";
import { Dispatcher } from "@/types";
import { UseCaseLike } from "@/usecases/type";
import { MessageNotification } from "@/domains/message-notification";

type Arg = {
  notification: MessageNotification;
};

export type UseCase = UseCaseLike<Actions, Arg>;

/**
 * Create the new use case
 */
export const createUseCase = function createUseCase(): UseCase {
  return {
    execute(dispatcher: Dispatcher<Actions>, arg: Arg) {
      const { notification } = arg;
      dispatcher.dispatch(actions.receiveMessage(notification));
    },
  };
};
