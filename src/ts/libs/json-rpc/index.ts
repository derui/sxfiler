import { Client, ClientImpl } from "./client";
import NotificationServer, { NotificationMethodMap } from "./notification-server";
import { IDGenerator } from "./type";
import { WebSocketHandler } from "./websocket-handler";
import WebSocketRequester from "./websocket-requester";

export interface Jsonrpc {
  handler: WebSocketHandler;
  requester: WebSocketRequester;
}

/**
 * initialize JSON-RPC base modules. Calling this function MUST be only once in application
 * @param ws WebSocket
 */
export function initialize(ws: WebSocket): Jsonrpc {
  const handler = new WebSocketHandler(ws);
  const requester = new WebSocketRequester(ws);

  handler.addHandler(requester);

  return {
    handler,
    requester,
  };
}

/**
 * Create notification server with base modules. Calling this function MUST be only once per jsonrpc module.
 * @param jsonrpc base module
 * @param methodMap method map for notification from server
 */
export function createNotificationServer(jsonrpc: Jsonrpc, methodMap: NotificationMethodMap): NotificationServer {
  const server = new NotificationServer(methodMap);

  jsonrpc.handler.addHandler(server);

  return server;
}

/**
 * Create JSON-RPC client to call or notify to server
 * @param jsonrpc base module
 * @param idGenerator ID generator
 */
export function createClient<M extends string>(jsonrpc: Jsonrpc, idGenerator: IDGenerator): Client<M> {
  return new ClientImpl(jsonrpc.requester, idGenerator);
}
