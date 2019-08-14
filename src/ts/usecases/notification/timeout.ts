import { Actions } from "@/actions";
import * as actions from "@/actions/notification";
import { Dispatcher } from "@/types";
import { UseCaseLike } from "@/usecases/type";

type Arg = {
  notificationId: string;
};

export type UseCase = UseCaseLike<Actions, Arg>;

/**
 * Create the new use case
 */
export const createUseCase = function createUseCase(): UseCase {
  return {
    execute(dispatcher: Dispatcher<Actions>, arg: Arg) {
      const { notificationId } = arg;
      dispatcher.dispatch(actions.timeout(notificationId));
    },
  };
};
