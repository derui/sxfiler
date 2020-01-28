import { Request, Response, Status, Error } from "@/generated/service_pb";
import { WebSocketHub } from "./websocket-hub";
import * as winston from "winston";
import { Loggers } from "@/loggers";

type Successed = {
  kind: "success";
  payload: Uint8Array;
};
type Failed = {
  kind: "failed";
  payload: Error;
};
type Ignored = {
  kind: "ignored";
};

export type ProcessResult = Successed | Failed | Ignored;

// helper function to create ProcessResult
export namespace ProcessResult {
  export const successed: (payload: Uint8Array) => ProcessResult = (payload) => ({ kind: "success", payload });
  export const failed: (payload: Error) => ProcessResult = (payload) => ({ kind: "failed", payload });
  export const ignored: () => ProcessResult = () => ({ kind: "ignored" });
}

export type ResponseSender = (result: ProcessResult) => Promise<void>;

/**
 * response handler for RPC. return payload for response if it processed.
 *
 * Argument `command` is member of `Command` enum in generated service, so you should
 * use `Command` enum in handler to switch process.
 *
 * Use lazyResponse function to send response to other.
 */
type Handler = (id: string, command: number, payload: Uint8Array, lazyResponse: ResponseSender) => void;

export interface RPCServer {
  /**
   * start RPC server for generated service
   */
  start(hub: WebSocketHub): void;

  /**
   * stop RPC server
   */
  stop(): void;

  /**
   * set a command handler to server. If you call this function twice with another function,
   * override handler that is setted before.
   */
  setCommandHandler(handler: Handler): void;
}

/**
 * create server instance.
 */
export const create = function create(): RPCServer {
  let commandHandler: Handler | null = null;

  // listener for RPC commonly
  const listener = (hub: WebSocketHub) => (message: string | ArrayBuffer) => {
    const logger = winston.loggers.get(Loggers.RPC);
    logger.info("Start request handling");

    let request: Request | null = null;

    if (typeof message === "string") {
      logger.error("Can not handle string because protobuf is binary format");
      return;
    }

    try {
      request = Request.deserializeBinary(new Uint8Array(message));
    } catch (e) {
      logger.debug(`can not decode message: %s`, e);
      return;
    }

    if (!request || !commandHandler) {
      return;
    }

    const id = request.getId();
    const command = request.getCommand();
    const payload = request.getPayload_asU8();

    const lazyResponse = async (res: ProcessResult) => {
      // create response
      const response = new Response();
      response.setId(id);

      switch (res.kind) {
        case "success":
          response.setStatus(Status.SUCCESS);
          response.setPayload(res.payload);
          break;
        case "failed":
          response.setStatus(Status.COMMAND_FAILED);
          response.setError(res.payload);
          break;
        case "ignored":
          logger.debug(`Do not return response from this command: ${id}`);
          return;
      }

      hub.send(response.serializeBinary());
      logger.info("Finish request handling");
    };

    commandHandler(id, command, payload, lazyResponse);
  };

  let unsubscribe: (() => void) | null = null;

  return {
    start(hub: WebSocketHub) {
      unsubscribe = hub.subscribe(listener(hub));
    },

    stop() {
      if (unsubscribe) {
        unsubscribe();
      }

      unsubscribe = null;
    },

    setCommandHandler(handler: Handler) {
      commandHandler = handler;
    },
  };
};
