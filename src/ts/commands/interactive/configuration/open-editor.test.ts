import * as C from "./open-editor";
import { createResolverMocks } from "@/commands/client-resolver-mock";
import { CommandState } from "@/commands/type";
import { actions } from "@/modules/configuration";

describe("Commands", () => {
  describe("interactive:configuration:Open Editor", () => {
    test("call action to open editor", () => {
      const command = C.createCommand();
      const mocks = createResolverMocks();

      command.execute(mocks.dispatcher, {} as CommandState, undefined);

      expect(mocks.dispatcher.dispatch).toBeCalledWith(actions.open());
    });
  });
});
