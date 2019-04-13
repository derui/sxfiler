import { Actions } from "../../actions";
import { actions } from "../../actions/ui-context";
import { Dispatcher } from "../../types";
import { UseCaseLike } from "../type";
import { Keymap } from "../../domains/keymap";

export type UseCase = UseCaseLike<Actions, { keymap: Keymap }>;

/**
 * Create the new use case instance
 */
export const createUseCase = (): UseCase => {
  return {
    execute(dispatcher: Dispatcher<Actions>, { keymap }: { keymap: Keymap }) {
      dispatcher.dispatch(actions.enablePreview({ keymap }));
    },
  };
};
