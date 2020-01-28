import * as C from "./cursor-up";
import { actions } from "@/modules/filer";

describe("Commands", () => {
  describe("interactive:filer:Cursor Up", () => {
    test("dispatch cursor up action of filer", async () => {
      const command = C.createCommand();

      const dispatcher = {
        dispatch: jest.fn(),
      };

      // do
      await command.execute(dispatcher, {} as any, undefined);

      // verify
      expect(dispatcher.dispatch).toBeCalledWith(actions.cursorUp());
    });
  });
});
