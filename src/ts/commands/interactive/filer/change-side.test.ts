import * as C from "./change-side";
import { actions } from "@/modules/filer";

describe("Commands", () => {
  describe("interactive:filer:Change Side", () => {
    test("dispatch change side action of filer", async () => {
      const command = C.createCommand();

      const dispatcher = {
        dispatch: jest.fn(),
      };

      await command.execute(dispatcher, {} as any, undefined);
      expect(dispatcher.dispatch).toBeCalledWith(actions.changeSide());
    });
  });
});
