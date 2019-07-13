import { actions } from "@/actions/task";
import { Dispatcher } from "@/dispatcher";
import { createUseCase } from "./finished";

describe("UseCases", () => {
  describe("Task", () => {
    describe("Require interaction", () => {
      it("dispatchs action", () => {
        const dispatcher = new Dispatcher();
        const fn = jest.fn();

        dispatcher.subscribe(fn);

        createUseCase().execute(dispatcher, "task");

        expect(fn.mock.calls[0]).toEqual([actions.finished("task")]);
      });
    });
  });
});
