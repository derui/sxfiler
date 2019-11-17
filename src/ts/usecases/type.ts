import { Dispatcher } from "@/types";

/**
 * Define use case that to use asynchrnous in use case
 */
export type SyncUseCaseLike<A, Extra extends {} = {}> = {
  /**
   * execute usecase with argument
   * @param arg
   */
  execute(dispatch: Dispatcher<A>, arg?: { [K in keyof Extra]: Extra[K] }): void;
};

/**
 * Define use case that to use asynchrnous in use case
 */
export type AsyncUseCaseLike<A, Extra extends {} = {}> = {
  /**
   * execute usecase with argument
   * @param arg
   */
  execute(dispatch: Dispatcher<A>, arg?: { [K in keyof Extra]: Extra[K] }): Promise<void>;
};

export type UseCaseLike<A, Extra extends {} = {}> = SyncUseCaseLike<A, Extra> | AsyncUseCaseLike<A, Extra>;
