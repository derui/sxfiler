import { actions } from "../../actions/task";
import { Dispatcher } from "../../dispatcher";
import { createUseCase } from "./require-interaction";
import { createInteraction } from "../../domains/task-interaction";

describe("UseCases", () => {
  describe("Task", () => {
    describe("Require interaction", () => {
      it("dispatchs action", () => {
        const dispatcher = new Dispatcher();
        const fn = jest.fn();

        dispatcher.subscribe(fn);
        const interaction = createInteraction({ id: "id", taskId: "task", acceptInteractions: [] });

        createUseCase().execute(dispatcher, { interaction });

        expect(fn.mock.calls[0]).toEqual([actions.requireInteraction(interaction)]);
      });
    });
  });
});
