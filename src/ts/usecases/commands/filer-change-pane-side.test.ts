import * as C from "./filer-change-pane-side";
import * as AppState from "@/states";
import { actions } from "@/actions/filer";

describe("Commands", () => {
  describe("Filer", () => {
    describe("Change the side of pane to use ", () => {
      it("call dispatch function", async () => {
        const command = C.createCommand();
        const dispatcher = {
          dispatch: jest.fn(),
        };

        await command.execute(dispatcher as any);
        expect(dispatcher.dispatch).toBeCalled();
      });

      it("change the side of pane", async () => {
        const command = C.createCommand();
        const dispatcher = {
          dispatch: jest.fn(),
        };
        const state = AppState.empty();

        await command.execute(dispatcher as any, { state: state, client: jest.fn() as any });
        expect(dispatcher.dispatch).toBeCalledWith(actions.changeSide());
      });
    });
  });
});
