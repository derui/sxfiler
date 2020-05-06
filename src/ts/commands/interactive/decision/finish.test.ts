import * as C from "./finish";
import * as M from "@/commands/client-resolver-mock";
import { actions } from "@/modules/decision";

describe("Commands", () => {
  describe("interactive:decision:Finish", () => {
    test("dispatch action to finish current processing", () => {
      const command = C.createCommand();
      const mocks = M.createResolverMocks();

      command.execute(mocks.dispatcher, { clientResolver: mocks.clientResolver } as any, undefined);
      expect(mocks.dispatcher.dispatch).toBeCalledTimes(1);
      expect(mocks.dispatcher.dispatch).toBeCalledWith(actions.finish());
    });
  });
});
