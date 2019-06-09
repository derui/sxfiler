import { actions } from "../../actions/task";
import { Dispatcher } from "../../dispatcher";
import { createUseCase } from "./update-reply-payload";
import { createOverwritePayload } from "../../domains/task-reply";

describe("UseCases", () => {
  describe("Task", () => {
    describe("Update reply payload", () => {
      it("throw error if argument is undefined", () => {
        const dispatcher = new Dispatcher();
        const fn = jest.fn();
        dispatcher.subscribe(fn);

        expect(() => createUseCase().execute(dispatcher, undefined)).toThrowError();
      });

      it("dispatchs action", () => {
        const dispatcher = new Dispatcher();
        const fn = jest.fn();
        dispatcher.subscribe(fn);

        const payload = createOverwritePayload();

        createUseCase().execute(dispatcher, payload);

        expect(fn.mock.calls[0]).toEqual([actions.updateReplyPayload(payload)]);
      });
    });
  });
});
