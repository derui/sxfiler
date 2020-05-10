import * as C from "./cancel";
import * as M from "@/commands/client-resolver-mock";
import { actions } from "@/modules/decision";

describe("Commands", () => {
  describe("interactive:decision:Cancel", () => {
    test("dispatch action to cancel current processing", () => {
      const command = C.createCommand();
      const mocks = M.createResolverMocks();

      command.execute(mocks.dispatcher, { clientResolver: mocks.clientResolver } as any, undefined);
      expect(mocks.dispatcher.dispatch).toBeCalledWith(actions.cancel());
    });
  });
});
