import { InteractionKind, createInteraction } from "./task-interaction";

describe("Domain", () => {
  describe("Task interaction", () => {
    it("can create interaction", () => {
      const data = createInteraction({
        id: "interactionId",
        taskId: "taskId",
        acceptInteractions: [InteractionKind.String],
      });

      expect(data.id).toEqual("interactionId");
      expect(data.taskId).toEqual("taskId");
      expect(data.acceptInteractions).toEqual([InteractionKind.String]);
    });
  });
});
