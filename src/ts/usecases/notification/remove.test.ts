import { actions } from "../../actions/notification";
import { Dispatcher } from "../../dispatcher";
import { createUseCase } from "./remove";

describe("UseCases", () => {
  describe("Notification", () => {
    describe("Remove", () => {
      it("dispatchs remove action", () => {
        const dispatcher = new Dispatcher();
        const fn = jest.fn();

        dispatcher.subscribe(fn);

        createUseCase().execute(dispatcher, { notificationId: "id" });

        expect(fn.mock.calls[0]).toEqual([actions.remove("id")]);
      });
    });
  });
});
