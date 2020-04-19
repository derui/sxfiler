import * as C from "./remove-context";
import * as M from "@/commands/client-resolver-mock";
import { CommandState } from "@/commands/type";
import { UIContext } from "@/types/ui-context";
import { actions } from "@/modules/keymap";

describe("Commands", () => {
  describe("internal:keymap:Remove Context", () => {
    test("dispatch action to add context", async () => {
      const mocks = M.createResolverMocks();

      const command = C.createCommand();
      await command.execute(mocks.dispatcher, {} as CommandState, { context: UIContext.OnFileTree });
      expect(mocks.dispatcher.dispatch).toBeCalledWith(actions.removeContexts([UIContext.OnFileTree]));
    });
  });
});
