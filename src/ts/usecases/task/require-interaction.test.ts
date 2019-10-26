import * as actions from "@/actions/task";
import { Dispatcher } from "@/dispatcher";
import { createUseCase } from "./require-interaction";
import { createSuggestions } from "@/domains/task-suggestion";

describe("UseCases", () => {
  describe("Task", () => {
    describe("Require interaction", () => {
      it("dispatchs action", () => {
        const dispatcher = new Dispatcher();
        const fn = jest.fn();

        dispatcher.subscribe(fn);
        const suggestions = createSuggestions({ taskId: "task", itemName: "node", suggestions: [] });

        createUseCase().execute(dispatcher, { suggestions });

        expect(fn.mock.calls[0]).toEqual([actions.requireInteraction(suggestions)]);
      });
    });
  });
});
