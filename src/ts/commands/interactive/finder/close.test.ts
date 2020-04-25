import * as C from "./close";
import * as M from "@/commands/client-resolver-mock";
import * as Keymap from "@/modules/keymap";
import * as Completer from "@/modules/completer";
import { CommandState } from "@/commands/type";
import { UIContext } from "@/types/ui-context";

describe("Commands", () => {
  describe("interactive:finder:Close", () => {
    test("call actions", () => {
      const mocks = M.createResolverMocks();
      const command = C.createCommand();

      command.execute(mocks.dispatcher, { clientResolver: mocks.clientResolver } as CommandState, undefined);

      expect(mocks.dispatcher.dispatch).toBeCalledWith(Keymap.actions.replaceContext([UIContext.OnFileTree]));
      expect(mocks.dispatcher.dispatch).toBeCalledWith(Completer.actions.close());
    });
  });
});
