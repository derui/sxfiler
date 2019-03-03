import UseCase from "./remove";
import { Dispatcher } from "../../dispatcher";
import { actions } from "../../actions/notification";

describe("UseCases", () => {
  describe('Notification', () => {
    describe('Remove', () => {
      it('dispatchs remove action', () => {
        const dispatcher = new Dispatcher();
        const fn = jest.fn();

        dispatcher.subscribe(fn);

        new UseCase().execute(dispatcher, { notificationId: "id" });

        expect(fn.mock.calls[0]).toEqual([actions.remove("id")]);
      });
    });
  });
});
