import * as C from "./update-file-window";
import * as M from "@/commands/client-resolver-mock";
import { FileWindow } from "@/generated/filer_pb";
import { Side } from "@/modules/filer/reducer";
import { State } from "@/modules";
import { actions } from "@/modules/filer";

describe("Commands", () => {
  describe("internal:filer:Update File Window", () => {
    test("dispatch action to update file window", async () => {
      const mocks = M.createResolverMocks();
      const command = C.createCommand();
      const state = {} as State;

      const fileWindow = new FileWindow();
      await command.execute(
        mocks.dispatcher,
        { clientResolver: mocks.clientResolver, state },
        {
          side: Side.Left,
          fileWindow,
        }
      );

      expect(mocks.dispatcher.dispatch).toBeCalledWith(actions.updateFileWindow(fileWindow, Side.Left));
    });
  });
});
