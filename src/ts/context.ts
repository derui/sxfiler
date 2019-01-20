import { Store } from "redux";

import { Actions } from "./actions";
import { Client } from "./libs/json-rpc/client";
import * as types from "./types/index";
import { StoreState } from "./types/store-state";
import {ApiMethod} from './apis';

// Context for application
export class Context {
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
}
