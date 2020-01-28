import { Request, Response, Status, Command } from "@/generated/service_pb";
import * as F from "../../generated/filer_pb";
import { create } from "./client";
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
        setTimeout(() => {
          e.emit("send", message);
        });
      }
    },
  };
};

describe("Library", () => {
  describe("WebSocket RPC", () => {
    describe("Server", () => {
      it("call handler when give message that is as request", async () => {
        // setup
        const em = new EventEmitter();
        const hub = createDummyHub(em);
        const client = create(hub);

        em.addListener("send", (data) => {
          const res = Request.deserializeBinary(data);
          expect(res.getId()).toBe("id");
          expect(res.getCommand()).toBe(Command.FILER_RELOAD_ALL);

          const response = also(new Response(), (v) => {
            v.setId("id");
            v.setStatus(Status.SUCCESS);
          });
          em.emit("receive", response.serializeBinary());
        });

        hub.start();

        // do
        const request = also(new Request(), (v) => {
          v.setId("id");
          v.setCommand(Command.FILER_RELOAD_ALL);
          const payload = new F.ReloadAllRequest().serializeBinary();
          v.setPayload(payload);
        });

        const res = await client.send(request);

        expect(res.getId()).toBe("id");
        expect(res.getStatus()).toBe(Status.SUCCESS);
      });
    });
  });
});
