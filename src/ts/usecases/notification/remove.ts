import { Actions } from "@/actions";
import { actions } from "@/actions/notification";
import { Dispatcher } from "@/types";
import { UseCaseLike } from "@/usecases/type";

type Arg = {
  notificationId: string;
};

export type UseCase = UseCaseLike<Actions, Arg>;

// An use case to remove notification from system
export const createUseCase = function createUseCase(): UseCase {
  return {
    execute(dispatcher: Dispatcher<Actions>, arg: Arg) {
      const { notificationId } = arg;
      dispatcher.dispatch(actions.remove(notificationId));
    },
  };
};
