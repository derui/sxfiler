import * as Actor from "./event-hub";

type EventTypes = "start" | "finish";
type Events =
  | {
      kind: "start";
      foo: string;
    }
  | { kind: "finish"; bar: number };

describe("Library", () => {
  describe("Actor", () => {
    test("call handler that is subscribed specific event", () => {
      const actor = Actor.create<EventTypes, Events>();

      actor.subscribe("finish", (e) => {
        switch (e.kind) {
          case "finish":
            expect(e.bar).toEqual(100);
            return;
          default:
            return;
        }
      });

      actor.publish({ kind: "finish", bar: 100 });
    });

    test("should not call handler if it already unsubscribe", () => {
      const actor = Actor.create<EventTypes, Events>();

      const handler = jest.fn();
      const unsubscribe = actor.subscribe("finish", handler);
      unsubscribe();

      actor.publish({ kind: "finish", bar: 100 });
      expect(handler).not.toBeCalled();
    });

    test("should call handler only once", () => {
      const actor = Actor.create<EventTypes, Events>();

      const handler = jest.fn();
      actor.once("finish", handler);

      actor.publish({ kind: "finish", bar: 100 });
      actor.publish({ kind: "finish", bar: 100 });
      expect(handler).toBeCalledTimes(1);
    });
  });
});
