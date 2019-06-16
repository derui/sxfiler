import { Actions } from "./actions";
import { ApiMethod } from "./apis";
import { Client } from "./libs/json-rpc/client";
import * as types from "./types/index";
import { UseCaseLike } from "./usecases/type";

// interface of context
export type ContextLike = {
  use<Param>(useCase: UseCaseLike<Actions, Param>): UseCaseExecutor<Param>;
};

export type UseCaseExecutor<Param> = {
  execute: (param: Param) => void;
};

// Context for application
class Context implements ContextLike {
  public readonly client: Client<ApiMethod>;
  public readonly dispatcher: types.Dispatcher<Actions>;

  /**
   * constructor of context
   * @param dispatcher dispatcher
   */
  public constructor(client: Client<ApiMethod>, dispatcher: types.Dispatcher<Actions>) {
    this.client = client;
    this.dispatcher = dispatcher;
  }

  /**
   * Register a use case to use next execution
   * @param useCase a use case to execute with this context
   * @return executor for the use case
   */
  public use<Param>(useCase: UseCaseLike<Actions, Param>): UseCaseExecutor<Param> {
    const dispatcter = this.dispatcher;
    return {
      execute(param: Param) {
        useCase.execute(dispatcter, param);
      },
    };
  }
}

/**
 * factory of Context
 */
export const createContext = ({
  client,
  dispatcher,
}: {
  client: Client<ApiMethod>;
  dispatcher: types.Dispatcher<Actions>;
}): ContextLike => {
  return new Context(client, dispatcher);
};
