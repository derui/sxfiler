import { Actions } from "../../actions";
import { actions } from "../../actions/notification";
import { Dispatcher } from "../../types";
import { UseCaseLike } from "../type";

interface Arg {
  notificationId: string;
}

export type UseCase = UseCaseLike<Actions, Arg>;

/**
 * Create the new use case
 */
export const createUseCase = (): UseCase => {
  return {
    execute(dispatcher: Dispatcher<Actions>, arg: Arg) {
      const { notificationId } = arg;
      dispatcher.dispatch(actions.timeout(notificationId));
    },
  };
};
