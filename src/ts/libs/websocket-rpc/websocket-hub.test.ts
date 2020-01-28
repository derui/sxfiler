import { create } from "./websocket-hub";
import { WebSocket, Server } from "mock-socket";

describe("Library", () => {
  describe("Websocket Event Hub", () => {
    const fakeURL = "ws://localhost:8080";
    let server = new Server(fakeURL);

    beforeEach(() => {
      server.start();
    });

    afterEach(() => {
      server.stop();
    });

    test("allow to subscribe websocket event", async () => {
      server.on("connection", (socket) => {
        socket.on("message", (data) => {
          expect(data).toBe("from client");
          socket.send("from server");
        });
      });

      const ws = new WebSocket(fakeURL);
      const hub = create(ws);

      hub.start();

      const promise = new Promise((resolve) => {
        hub.subscribe((message) => {
          expect(message).toBe("from server");
          resolve();
        });
      });

      hub.send("from client");

      await promise;
      hub.stop();
    });

    test("do not subscribe any events if it unsubscribed", () => {
      server.on("connection", (socket) => {
        socket.on("message", (data) => {
          expect(data).toBe("from client");
          socket.send("from server");
        });
      });

      const ws = new WebSocket(fakeURL);
      const hub = create(ws);

      hub.start();
      const unsubscribe = hub.subscribe(() => {
        fail("invalid path");
      });
      unsubscribe();

      hub.send("from client");
      hub.stop();
    });
  });
});
