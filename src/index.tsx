import bigInt from "big-integer";
import Int64 from "node-int64";
import * as React from "react";
import * as ReactDOM from "react-dom";
import { createStore } from "redux";

import { Actions } from "./ts/actions";
import { ApiMethod } from "./ts/apis";
import App from "./ts/app";
import { Context } from "./ts/context";
import Dispatcher from "./ts/dispatcher";
import * as jrpc from "./ts/libs/json-rpc";
import { Client } from "./ts/libs/json-rpc/client";
import { setLocator } from "./ts/locator";
import reducer from "./ts/reducers";

const url = process.env.NODE_ENV === "production" ? process.env.REACT_APP_SERVER : "ws://localhost:50879";

const ws = new WebSocket(url || "");
const jsonrpc = jrpc.initialize(ws);

const client: Client<ApiMethod> = jrpc.createClient(jsonrpc, () => {
  return bigInt.randBetween(bigInt(Int64.MIN_INT), bigInt(Int64.MAX_INT)).toString();
});

const store = createStore(reducer);

ws.onopen = () => {
  /* TODO: call methods to initialize front */
};

const dispatcher: Dispatcher<Actions> = new Dispatcher();
dispatcher.subscribe(store.dispatch);

const locator = {
  context: new Context(client, dispatcher, store),
};
setLocator(locator);

function render() {
  ReactDOM.render(<App />, document.getElementById("root") as HTMLElement);
}

render();
