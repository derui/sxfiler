import * as C from "./reset";
import * as M from "@/commands/client-resolver-mock";
import { actions } from "@/modules/decision";

describe("Commands", () => {
  describe("internal:decision:Reset", () => {
    test("dispatch action to reset all state for decision", () => {
      const command = C.createCommand();
      const mocks = M.createResolverMocks();

      command.execute(mocks.dispatcher, { clientResolver: mocks.clientResolver } as any, undefined);
      expect(mocks.dispatcher.dispatch).toBeCalledTimes(1);
      expect(mocks.dispatcher.dispatch).toBeCalledWith(actions.reset());
    });
  });
});
