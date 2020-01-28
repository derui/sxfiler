import { Request, Response } from "@/generated/service_pb";
import { WebSocketHub } from "./websocket-hub";
import * as winston from "winston";
import { Loggers } from "@/loggers";

export type RPCClient = {
  /**
   * send a request to server.
   */
  send(request: Request): Promise<Response>;

  /**
   * destroy client.
   */
  destroy(): void;
};

type Callback = (resonse: Response) => void;

/**
   Create a RPC Impl for any service genereated from proto file.
 */
export const create = function create(hub: WebSocketHub): RPCClient {
  const waitingRequests = new Map<string, Callback>();

  const unsubscribe = hub.subscribe((message) => {
    const logger = winston.loggers.get(Loggers.RPC);
    logger.info(`Start handling response for some request`);
    if (typeof message === "string") {
      logger.warn("Can not handle string because this implementation allows only binary format");
      return;
    }

    try {
      const protobuf = new Uint8Array(message);
      const response = Response.deserializeBinary(protobuf);

      const callback = waitingRequests.get(response.getId());

      if (!callback) {
        logger.debug(`Detect unhandled request: ${response.getId()}`);
        return;
      }
      waitingRequests.delete(response.getId());
      callback(response);
      logger.info(`Finish handling response for ID: ${response.getId()}`);
    } catch (e) {
      logger.warn(`Detect error in RPC: ${e}`);
    }
  });

  return {
    send(request: Request): Promise<Response> {
      // get id of request
      const currentId = request.getId();

      const promise: Promise<Response> = new Promise((resolve) => {
        waitingRequests.set(currentId, resolve);
        hub.send(request.serializeBinary());
      });
      return promise;
    },

    destroy() {
      unsubscribe();
    },
  };
};
