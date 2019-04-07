import { Dispatcher } from "../types";
import { Actions } from "../actions";
import { AppState } from "../states";

export type UseCaseLike<A, Extra extends {} = {}> = {
  /**
   * execute usecase with argument
   * @param arg
   */
  execute(dispatch: Dispatcher<A>, arg?: { [K in keyof Extra]: Extra[K] }): void;
};

/**
 * A type of commands in this application.
 */
export type CommandLike = UseCaseLike<
  Actions,
  {
    state: AppState;
  }
>;
