import { Store } from "redux";

import Actions from "./actions/types";
import * as types from "./types/index";
import {StoreState} from "./types/store-state";

// Context for application
export default class Context {
  /**
   * constructor of context
   * @param dispatcher dispatcher
   * @param store store
   */
  constructor(
    public readonly dispatcher: types.Dispatcher<Actions>,
    public readonly store: Store<StoreState>
  ) {}
}
