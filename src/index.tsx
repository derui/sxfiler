import bigInt from "big-integer";
import Int64 from "node-int64";
import * as React from "react";
import * as ReactDOM from "react-dom";
import { createStore, Store } from "redux";
import * as kbd from "@/libs/kbd";

import { Actions } from "./ts/actions";
import { ApiMethod } from "./ts/apis";
import { Component as App } from "./ts/app";
import { createContext } from "./ts/context";
import { Dispatcher } from "./ts/dispatcher";
import * as jrpc from "./ts/libs/json-rpc";
import { LocatorContext, Locator } from "./ts/locator";
import { reducer } from "./ts/reducers";
import { AppState } from "./ts/states";

import { createUseCase } from "./ts/usecases/filer/initialize";
import { createCommandRegistrar } from "./ts/usecases/command-registrar";
import { registAllCommand } from "./ts/usecases/commands";
import * as Get from "./ts/usecases/keymap/get";
import * as List from "./ts/usecases/bookmark/list";
import * as NotificationHandlers from "./ts/notification-handlers";
import { ModalRootContext } from "./ts/modal-root";
import { findBinding } from "@/states/keymap";

declare var window: Window & {
  applicationConfig: {
    serverURL: string;
  };
};

const url = process.env.NODE_ENV === "production" ? window.applicationConfig.serverURL : "ws://localhost:50879";

const ws = new WebSocket(url || "");
const jsonrpc = jrpc.initialize(ws);

const client = jrpc.createClient<ApiMethod>(jsonrpc, () => {
  return bigInt.randBetween(bigInt(Int64.MIN_INT), bigInt(Int64.MAX_INT)).toString();
});

const store = createStore(reducer);

const dispatcher = new Dispatcher<Actions>();
dispatcher.subscribe(store.dispatch);

const locator = {
  context: createContext({ client, dispatcher }),
  client,
  commandRegistrar: registAllCommand(createCommandRegistrar(client)),
};

// register notification handlers
jrpc.createNotificationServer(jsonrpc, locator.context, {
  "notification/message": NotificationHandlers.handleMessageNotification,
  "notification/progress": NotificationHandlers.handleProgressNotification,
  "notification/taskFinished": NotificationHandlers.handleTaskFinished,
  "notification/taskInteraction": NotificationHandlers.handleTaskInteraction,
  "notification/filerUpdated": NotificationHandlers.handleFilerUpdated,
});

const initializeState = () => {
  locator.context.use(Get.createUseCase(client))({});
  locator.context.use(createUseCase(client))({ location: "." });
  locator.context.use(List.createUseCase(client))({});
};

ws.onopen = () => {
  initializeState();
};

const render = function render(state: AppState) {
  ReactDOM.render(
    <ModalRootContext.Provider value={{ element: document.getElementById("modal-root") }}>
      <LocatorContext.Provider value={locator}>
        <App state={state} />
      </LocatorContext.Provider>
    </ModalRootContext.Provider>,
    document.getElementById("root") as HTMLElement
  );
};

store.subscribe(() => {
  render(store.getState());
});

/**
 * handle keyboard event that all keydown event on application
 * @param props properties of component
 */
const handleKeyDown = function handleKeyDown(locator: Locator, store: Store<AppState, Actions>) {
  return (ev: KeyboardEvent) => {
    const state = store.getState();
    const { context, commandRegistrar } = locator;

    switch (ev.type) {
      case "keydown": {
        const key = kbd.make(ev.key, { meta: ev.metaKey, ctrl: ev.ctrlKey });
        const binding = findBinding(state.keymap, state.context, kbd.toKeySeq(key));

        if (!binding) {
          break;
        }

        ev.preventDefault();
        ev.stopPropagation();
        if (context && commandRegistrar) {
          commandRegistrar.execute(binding.action, context, { state });
        }
        break;
      }
      default:
        break;
    }

    return;
  };
};

document.body.addEventListener("keydown", ev => handleKeyDown(locator, store)(ev));
