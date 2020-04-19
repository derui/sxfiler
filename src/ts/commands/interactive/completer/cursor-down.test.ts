import * as C from "./cursor-down";
import { actions } from "@/modules/completer";

describe("Commands", () => {
  describe("interactive:completer:Cursor Down", () => {
    test("dispatch cursor down action of completer", async () => {
      const command = C.createCommand();

      const dispatcher = {
        dispatch: jest.fn(),
      };

      await command.execute(dispatcher, {} as any, undefined);
      expect(dispatcher.dispatch).toBeCalledWith(actions.cursorDown());
    });
  });
});
