import * as C from "./apply-file-events";
import * as M from "@/commands/client-resolver-mock";
import { State } from "@/modules";
import { actions } from "@/modules/filer";

describe("Commands", () => {
  describe("internal:filer:Update File Window", () => {
    test("dispatch action to update file window", async () => {
      const mocks = M.createResolverMocks();
      const command = C.createCommand();
      const state = {} as State;

      await command.execute(
        mocks.dispatcher,
        { clientResolver: mocks.clientResolver, state },
        {
          fileListId: "id",
          events: [],
          itemOrders: [],
        }
      );

      expect(mocks.dispatcher.dispatch).toBeCalledWith(actions.applyEvents("id", []));
    });
  });
});
