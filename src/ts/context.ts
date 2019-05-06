import { Store } from "redux";

import { Actions } from "./actions";
import { ApiMethod } from "./apis";
import { Client } from "./libs/json-rpc/client";
import * as types from "./types/index";
import { AppState } from "./states";
import { UseCaseLike } from "./usecases/type";

// interface of context
export type ContextLike = {
  execute<Param>(usecase: UseCaseLike<Actions, Param>, arg: Param): void;
};

// Context for application
export class Context implements ContextLike {
  public readonly client: Client<ApiMethod>;
  public readonly dispatcher: types.Dispatcher<Actions>;
  public readonly store: Store<AppState, Actions>;

  /**
   * constructor of context
   * @param dispatcher dispatcher
   * @param store store
   */
  public constructor(
    client: Client<ApiMethod>,
    dispatcher: types.Dispatcher<Actions>,
    store: Store<AppState, Actions>
  ) {
    this.client = client;
    this.dispatcher = dispatcher;
    this.store = store;
  }

  /**
   * execute use case with context
   * @param usecase an usecase to run
   * @param arg the argument to pass use case
   */
  public execute<Param>(usecase: UseCaseLike<Actions, Param>, arg: Param): void {
    usecase.execute(this.dispatcher, arg);
  }
}
