import { Store } from "redux";

import * as actions from "./actions/index";
import * as types from "./types/index";

// Context for application
export default class Context {
  /**
   * constructor of context
   * @param dispatcher dispatcher
   * @param store store
   */
  constructor(
    public readonly dispatcher: types.Dispatcher<actions.Action>,
    public readonly store: Store<types.StoreState, actions.Action>
  ) {}
}
