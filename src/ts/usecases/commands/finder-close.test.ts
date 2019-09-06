import * as C from "./finder-close";
import * as AppState from "@/states";
import * as actions from "@/actions/completer";
import { UIContext } from "@/types/ui-context";

describe("Commands", () => {
  describe("Completer", () => {
    describe("Close completion", () => {
      it("throw error when pass undefined as argument", async () => {
        const command = C.createCommand();
        const dispatcher = jest.fn();

        await expect(command.execute(dispatcher as any, undefined)).rejects.toThrowError();
      });

      it("should dispatch close action", async () => {
        const command = C.createCommand();
        const dispatcher = {
          dispatch: jest.fn(),
        };
        const client = {
          call: jest.fn(),
        };

        const state = AppState.empty();

        await command.execute(dispatcher as any, { state, client: client as any });
        expect(dispatcher.dispatch).toBeCalledWith(actions.close(UIContext.ForFinder));
      });
    });
  });
});
