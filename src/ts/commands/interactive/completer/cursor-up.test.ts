import * as C from "./cursor-up";
import { actions } from "@/modules/completer";

describe("Commands", () => {
  describe("interactive:completer:Cursor Up", () => {
    test("dispatch cursor up action of completer", async () => {
      const command = C.createCommand();

      const dispatcher = {
        dispatch: jest.fn(),
      };

      await command.execute(dispatcher, {} as any, undefined);

      expect(dispatcher.dispatch).toBeCalledWith(actions.cursorUp());
    });
  });
});
