import * as C from "./filer-move-cursor-up";
import * as AppState from "../../states";

describe("Commands", () => {
  describe("Filer", () => {
    describe("Move the cursor up of the filer", () => {
      it("throw error when pass undefined as argument", () => {
        const command = C.createCommand();
        const dispatcher = jest.fn();

        expect(() => command.execute(dispatcher as any, undefined)).toThrowError();
      });

      it("do not call anything when filer is not initialized", () => {
        const command = C.createCommand();
        const dispatcher = {
          dispatch: jest.fn(),
        };
        const state = AppState.empty();
        command.execute(dispatcher as any, { state: state });
        expect(dispatcher.dispatch).not.toBeCalled();
      });
    });
  });
});
