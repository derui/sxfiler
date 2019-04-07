import { Actions } from "../../actions";
import { actions } from "../../actions/ui-context";
import { Dispatcher } from "../../types";
import { UseCaseLike } from "../type";

export type UseCase = UseCaseLike<Actions>;

/**
 * Create the new use case instance
 */
export const createUseCase = (): UseCase => {
  return {
    execute(dispatcher: Dispatcher<Actions>) {
      dispatcher.dispatch(actions.enablePreview());
    },
  };
};
