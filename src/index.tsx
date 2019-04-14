import bigInt from "big-integer";
import Int64 from "node-int64";
import * as React from "react";
import * as ReactDOM from "react-dom";
import { createStore } from "redux";

import { Actions } from "./ts/actions";
import { ApiMethod } from "./ts/apis";
import { Component as App } from "./ts/app";
import { Context } from "./ts/context";
import { Dispatcher } from "./ts/dispatcher";
import * as jrpc from "./ts/libs/json-rpc";
import { Client } from "./ts/libs/json-rpc/client";
import LocatorContext from "./ts/locator";
import reducer from "./ts/reducers";
import { AppState } from "./ts/states";

import { createUseCase } from "./ts/usecases/filer/initialize";
import { createCommandRegistrar } from "./ts/usecases/command-registrar";
import { registAllCommand } from "./ts/usecases/commands";
import * as enableFileTree from "./ts/usecases/ui-context/enable-file-tree";

const url = process.env.NODE_ENV === "production" ? process.env.REACT_APP_SERVER : "ws://localhost:50879";

const ws = new WebSocket(url || "");
const jsonrpc = jrpc.initialize(ws);

const client: Client<ApiMethod> = jrpc.createClient(jsonrpc, () => {
  return bigInt.randBetween(bigInt(Int64.MIN_INT), bigInt(Int64.MAX_INT)).toString();
});

const store = createStore(reducer);

const dispatcher: Dispatcher<Actions> = new Dispatcher();
dispatcher.subscribe(store.dispatch);

const locator = {
  context: new Context(client, dispatcher, store),
  commandRegistrar: registAllCommand(createCommandRegistrar(client)),
};

const initializeState = () => {
  locator.context.execute(createUseCase(client), { location: "." });
  locator.context.execute(enableFileTree.createUseCase(client), {});
};

ws.onopen = () => {
  initializeState();
};

function render(state: AppState) {
  ReactDOM.render(
    <LocatorContext.Provider value={locator}>
      <App state={state} />
    </LocatorContext.Provider>,
    document.getElementById("root") as HTMLElement
  );
}

store.subscribe(() => {
  render(store.getState());
});
