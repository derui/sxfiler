import { createOverwritePayload, createRenamePayload, createReply } from "./task-reply";
import { SuggestionKind } from "./task-suggestion";

describe("Domain", () => {
  describe("Task reply", () => {
    it("can create payload to overwrite a node", () => {
      const data = createOverwritePayload();

      expect(data.kind).toEqual(SuggestionKind.Overwrite);
    });

    it("can create payload to rename a node", () => {
      const data = createRenamePayload("foo");

      switch (data.kind) {
        case SuggestionKind.Rename:
          expect(data.newName).toEqual("foo");
          break;
        default:
          fail();
      }
    });

    it("can create reply", () => {
      const payload = createOverwritePayload();
      const data = createReply("task", payload);

      expect(data.taskId).toEqual("task");
      expect(data.reply).toEqual(payload);
    });
  });
});
