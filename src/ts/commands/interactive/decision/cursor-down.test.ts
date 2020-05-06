import * as C from "./cursor-down";
import * as M from "@/commands/client-resolver-mock";
import { CommandState } from "@/commands/type";
import { actions } from "@/modules/decision";

describe("Commands", () => {
  describe("interactive:decision:Cursor Down", () => {
    test("call action to select next action", () => {
      const mocks = M.createResolverMocks();

      const command = C.createCommand();

      command.execute(mocks.dispatcher, {} as CommandState, undefined);

      expect(mocks.dispatcher.dispatch).toBeCalledWith(actions.selectNextAction());
    });
  });
});
