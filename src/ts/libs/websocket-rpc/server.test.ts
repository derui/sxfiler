import { Request, Response, Status, Command, Error } from "@/generated/service_pb";
import * as F from "../../generated/filer_pb";
import { create, ProcessResult } from "./server";
import * as WebSocketHub from "./websocket-hub";
import { EventEmitter } from "events";
import { also } from "../fn";

const createDummyHub = function (e: EventEmitter): WebSocketHub.WebSocketHub {
  let subscriber: WebSocketHub.Subscriber | null = null;
  return {
    start() {
      e.addListener("receive", (data) => {
        if (subscriber) {
          subscriber(data);
        }
      });
    },
    stop() {},
    subscribe(f: WebSocketHub.Subscriber): WebSocketHub.Unsubscribe {
      subscriber = f;
      return () => {
        subscriber = null;
      };
    },

    send(message: string | ArrayBuffer) {
      if (subscriber) {
        e.emit("send", message);
      }
    },
  };
};

describe("Library", () => {
  describe("WebSocket RPC", () => {
    describe("Server", () => {
      it("call handler when give message that is as request", () => {
        const em = new EventEmitter();
        const hub = createDummyHub(em);
        const server = create();
        const request = also(new Request(), (v) => {
          v.setId("id");
          v.setCommand(Command.FILER_RELOAD_ALL);
          const payload = new F.ReloadAllRequest().serializeBinary();
          v.setPayload(payload);
        });

        return new Promise((resolve) => {
          server.setCommandHandler((id: string, command: number, payload: Uint8Array, lazyResponse) => {
            expect(id).toEqual("id");
            expect(command).toBe(Command.FILER_RELOAD_ALL);

            const req = F.ReloadAllRequest.deserializeBinary(payload);
            expect(req).not.toBeNull();

            const res = new F.ReloadAllResponse();
            lazyResponse(ProcessResult.successed(res.serializeBinary()));
          });

          em.addListener("send", (data) => {
            const res = Response.deserializeBinary(data);
            expect(res.getId()).toBe("id");
            expect(res.getStatus()).toBe(Status.SUCCESS);
            resolve();
          });

          hub.start();
          server.start(hub);

          em.emit("receive", request.serializeBinary());
        });
      });

      it("do not anything when handler return undefined", async () => {
        const em = new EventEmitter();
        const hub = createDummyHub(em);
        const server = create();
        const request = also(new Request(), (v) => {
          v.setId("id");
          v.setCommand(Command.FILER_RELOAD_ALL);
          v.setPayload(new F.ReloadAllRequest().serializeBinary());
        });

        server.setCommandHandler(() => {
          return Promise.resolve(undefined);
        });

        em.addListener("send", () => {
          fail("invalid path");
        });

        hub.start();
        server.start(hub);

        em.emit("receive", request.serializeBinary());

        await Promise.resolve();
      });
    });
  });
});
