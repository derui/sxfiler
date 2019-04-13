import * as C from "./filer-change-pane-side";
import * as AppState from "../../states";
import { actions } from "../../actions/filer";

describe("Commands", () => {
  describe("Filer", () => {
    describe("Change the side of pane to use ", () => {
      it("call dispatch function", () => {
        const command = C.createCommand();
        const dispatcher = {
          dispatch: jest.fn(),
        };

        command.execute(dispatcher as any);
        expect(dispatcher.dispatch).toBeCalled();
      });

      it("change the side of pane", () => {
        const command = C.createCommand();
        const dispatcher = {
          dispatch: jest.fn(),
        };
        const state = AppState.empty();

        command.execute(dispatcher as any, { state: state });
        expect(dispatcher.dispatch).toBeCalledWith(actions.changeSide());
      });
    });
  });
});
