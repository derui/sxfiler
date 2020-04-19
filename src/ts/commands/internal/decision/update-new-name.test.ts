import * as C from "./update-new-name";
import * as Mock from "@/commands/client-resolver-mock";
import { CommandState } from "@/commands/type";
import { actions } from "@/modules/decision";

describe("Commands", () => {
  describe("internal:decision:Update New Name", () => {
    test("dispatch action to update new name", async () => {
      const mock = Mock.createResolverMocks();
      const command = C.createCommand();
      await command.execute(mock.dispatcher, { clientResolver: mock.clientResolver } as CommandState, {
        updatedName: "name",
      });

      expect(mock.dispatcher.dispatch).toBeCalledWith(actions.updateNewName("name"));
    });
  });
});
