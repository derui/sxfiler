import { actions } from "../../actions/notification";
import { Dispatcher } from "../../dispatcher";
import UseCase from "./timeout";

describe("UseCases", () => {
  describe("Notification", () => {
    describe("Timeout", () => {
      it("dispatchs action", () => {
        const dispatcher = new Dispatcher();
        const fn = jest.fn();

        dispatcher.subscribe(fn);

        new UseCase().execute(dispatcher, { notificationId: "id" });

        expect(fn.mock.calls[0]).toEqual([actions.timeout("id")]);
      });
    });
  });
});
