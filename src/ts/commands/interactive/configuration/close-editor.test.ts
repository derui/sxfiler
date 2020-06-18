import * as C from "./close-editor";
import { createResolverMocks } from "@/commands/client-resolver-mock";
import { CommandState } from "@/commands/type";
import { actions } from "@/modules/configuration";

describe("Commands", () => {
  describe("interactive:configuration:Close Editor", () => {
    test("call action to close editor", () => {
      const command = C.createCommand();
      const mocks = createResolverMocks();

      command.execute(mocks.dispatcher, {} as CommandState, undefined);

      expect(mocks.dispatcher.dispatch).toBeCalledWith(actions.close());
    });
  });
});
