import { Store } from "redux";

import { Actions } from "./actions";
import { ApiMethod } from "./apis";
import { Client } from "./libs/json-rpc/client";
import * as types from "./types/index";
import { StoreState } from "./types/store-state";
import { UseCaseLike } from "./usecases/type";

// interface of context
export interface ContextLike {
  execute<Param>(usecase: UseCaseLike<Actions, Param>, arg: Param): void;
}

// Context for application
export class Context implements ContextLike {
  /**
   * constructor of context
   * @param dispatcher dispatcher
   * @param store store
   */
  constructor(
    public readonly client: Client<ApiMethod>,
    public readonly dispatcher: types.Dispatcher<Actions>,
    public readonly store: Store<StoreState, Actions>
  ) {}

  /**
   * execute use case with context
   * @param usecase an usecase to run
   * @param arg the argument to pass use case
   */
  public execute<Param>(usecase: UseCaseLike<Actions, Param>, arg: Param) {
    usecase.execute(this.dispatcher, arg);
  }
}
