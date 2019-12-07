import * as actions from "@/actions/task";
import { Dispatcher } from "@/dispatcher";
import { createUseCase } from "./canceled";

describe("UseCases", () => {
  describe("Task", () => {
    describe("canceled", () => {
      it("dispatchs action", () => {
        const dispatcher = new Dispatcher();
        const fn = jest.fn();

        dispatcher.subscribe(fn);

        createUseCase().execute(dispatcher, "task");

        expect(fn.mock.calls[0]).toEqual([actions.canceled("task")]);
      });
    });
  });
});
