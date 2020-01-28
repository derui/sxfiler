import * as C from "./cursor-down";
import { actions } from "@/modules/filer";

describe("Commands", () => {
  describe("interactive:filer:Cursor Down", () => {
    test("dispatch cursor down action of filer", async () => {
      const command = C.createCommand();

      const dispatcher = {
        dispatch: jest.fn(),
      };

      // do
      await command.execute(dispatcher, {} as any, undefined);

      // verify
      expect(dispatcher.dispatch).toBeCalledWith(actions.cursorDown());
    });
  });
});
