import { Dispatcher } from "../types";

export interface UseCaseLike<A, Extra extends {} = {}> {
  /**
   * execute usecase with argument
   * @param arg
   */
  execute(dispatch: Dispatcher<A>, arg?: { [K in keyof Extra]: Extra[K] }): void;
}
