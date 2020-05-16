import { h, render } from "preact";
import { createStore, Store, applyMiddleware } from "redux";
import * as kbd from "@/libs/kbd";

import { Actions } from "@/modules";
import { Component as App } from "@/app";
import { createContext } from "@/context";
import { Dispatcher } from "@/dispatcher";
import { LocatorContext, Locator } from "@/locator";
import { reducer, State } from "@/modules";

import * as Commands from "@/commands";
import { ModalRootContext } from "./ts/modal-root";
import * as WsRpc from "@/libs/websocket-rpc";
import * as Rpc from "@/rpc/client";
import { notificationHandler } from "@/rpc/notification-handler";
import { descriptors } from "@/commands/interactive";
import { descriptors as internalDescriptors } from "@/commands/internal";
import { initializeLoggers, Loggers } from "@/loggers";
import * as winston from "winston";
import { UIContext } from "@/types/ui-context";
import * as CommandExecutor from "@/commands/command-executor";
import * as TypedEventHub from "@/typed-event-hub";
import * as GlobalEventPublisher from "@/global-event-publisher";

declare var window: Window & {
  ipcRenderer: any;
  applicationConfig: {
    serverURL: string;
  };
};

// initialize logger
initializeLoggers(process.env.NODE_ENV !== "production");

const url = process.env.NODE_ENV === "production" ? window.applicationConfig.serverURL : "ws://localhost:50789";
// setup global store
const eventHub = TypedEventHub.create();
const store = createStore(reducer, applyMiddleware(GlobalEventPublisher.create(eventHub)));

// setup WebSocket-based RPC
const ws = new WebSocket(url || "");
ws.binaryType = "arraybuffer";
const wsHub = WsRpc.WebSocketHub.create(ws);
const clientImpl = WsRpc.Client.create(wsHub);
const rpcClient = Rpc.create(clientImpl);

// setup context and locator
const dispatcher = new Dispatcher<Actions>();
dispatcher.subscribe(store.dispatch);

const commandExecutor = CommandExecutor.create(
  {
    rpcClient: () => rpcClient,
    appClient() {
      return {
        quit() {
          // send quit event to main process
          window.ipcRenderer.send("quit");
        },
      };
    },
  },
  createContext({ dispatcher })
);

const locator = {
  commandExecutor,
  commandResolver: Commands.makeWiredResolver(),
};

const wiredResolver = Commands.makeWiredResolver();

// set up notification server
const server = WsRpc.Server.create();
server.setCommandHandler(
  notificationHandler({
    getState: store.getState,
    getCommandResolver() {
      return wiredResolver;
    },
    getCommandExecutor() {
      return commandExecutor;
    },
    getSubscriber() {
      return eventHub;
    },
  })
);
server.start(wsHub);

wsHub.start();

ws.onopen = () => {
  const reloadAll = wiredResolver.resolveBy(descriptors.filerReloadAll);
  const getKeymap = wiredResolver.resolveBy(descriptors.keymapReload);
  const addContext = wiredResolver.resolveBy(internalDescriptors.keymapAddContext);
  if (reloadAll && getKeymap && addContext) {
    commandExecutor.execute(addContext, store.getState(), { context: UIContext.OnFileTree });
    commandExecutor.execute(reloadAll, store.getState(), undefined);
    commandExecutor.execute(getKeymap, store.getState(), undefined);
  }

  const getConfiguration = wiredResolver.resolveBy(internalDescriptors.configurationInitialize);
  if (getConfiguration) {
    commandExecutor.execute(getConfiguration, store.getState(), undefined);
  }
};

const renderApp = (state: State) => {
  render(
    <ModalRootContext.Provider value={{ element: document.getElementById("modal-root") }}>
      <LocatorContext.Provider value={locator}>
        <App state={state} />
      </LocatorContext.Provider>
    </ModalRootContext.Provider>,
    document.getElementById("root") as HTMLElement
  );
};

store.subscribe(() => {
  renderApp(store.getState());
});

/**
 * handle keyboard event that all keydown event on application
 * @param props properties of component
 */
const handleKeyDown = function handleKeyDown(locator: Locator, store: Store<State, Actions>) {
  return (ev: KeyboardEvent) => {
    const logger = winston.loggers.get(Loggers.KEY_EVENT);
    logger.debug("Start handling key event", ev);

    const state = store.getState();
    const { commandExecutor, commandResolver } = locator;

    switch (ev.type) {
      case "keydown": {
        const key = kbd.make(ev.key, { meta: ev.metaKey, ctrl: ev.ctrlKey });
        const binding = state.keymap.currentKeymap[kbd.toKeySeq(key)];

        if (!binding) {
          logger.debug(`Not found binding for key seq: ${JSON.stringify(key)}`);
          break;
        }

        ev.preventDefault();
        ev.stopPropagation();

        if (commandExecutor && commandResolver) {
          logger.debug(`Resolve command having identifier: ${binding}`);
          const command = commandResolver.resolveById(binding);
          if (!command) {
            logger.warn(`Can not resolve command: ${binding}`);
            return;
          }
          commandExecutor.execute(command, state, undefined);
        }
        break;
      }
      default:
        break;
    }

    return;
  };
};

document.body.addEventListener("keydown", handleKeyDown(locator, store));
