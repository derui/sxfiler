import { createYesNoInteraction, createStringInteraction, createIntInteraction } from "./task-payload";
import { InteractionKind } from "./task-interaction";

describe("Domain", () => {
  describe("Task payload", () => {
    it("can create payload for Yes/No interaction", () => {
      const data = createYesNoInteraction(true);

      expect(data.kind).toEqual(InteractionKind.YesNo);
    });

    it("can create payload for interaction string", () => {
      const data = createStringInteraction("foo");

      expect(data.kind).toEqual(InteractionKind.String);
      expect(data.payload).toEqual("foo");
    });

    it("can create payload for interaction number", () => {
      const data = createIntInteraction(100);

      expect(data.kind).toEqual(InteractionKind.Int);
      expect(data.payload).toEqual(100);
    });

    it("can convert payload to server representation", () => {
      const data = createStringInteraction("foo");

      expect(data.toServerRepresentation()).toEqual([InteractionKind.String, "foo"]);
    });
  });
});
