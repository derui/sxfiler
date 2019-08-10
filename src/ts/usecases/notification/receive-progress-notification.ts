import { Actions } from "@/actions";
import { actions } from "@/actions/notification";
import { Dispatcher } from "@/types";
import { UseCaseLike } from "@/usecases/type";
import { ProgressNotification } from "@/domains/progress-notification";

interface Arg {
  notification: ProgressNotification;
}

export type UseCase = UseCaseLike<Actions, Arg>;

/**
 * Create the new use case
 */
export const createUseCase = (): UseCase => {
  return {
    execute(dispatcher: Dispatcher<Actions>, arg: Arg) {
      const { notification } = arg;
      dispatcher.dispatch(actions.receiveProgress(notification));

      if (notification.body.current >= notification.body.targeted) {
        dispatcher.dispatch(actions.remove(notification.id));
      }
    },
  };
};
