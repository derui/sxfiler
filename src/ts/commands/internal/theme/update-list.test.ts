import * as C from "./update-list";
import { createResolverMocks } from "@/commands/client-resolver-mock";
import { CommandState } from "@/commands/type";
import * as P from "@/rpc/client-procedures";
import { ListResponse, ListRequest } from "@/generated/theme_pb";
import { actions } from "@/modules/theme";

describe("Commands", () => {
  describe("internal:theme:Update List", () => {
    test("call action with all list of theme", async () => {
      const mocks = createResolverMocks();
      const command = C.createCommand();
      const mockForUse = jest.fn();
      const res = new ListResponse();
      mockForUse.mockImplementation(() => Promise.resolve(res));
      mocks.rpcClient.use.mockImplementation(() => mockForUse);

      const expected = new ListRequest();

      await command.execute(mocks.dispatcher, { clientResolver: mocks.clientResolver } as CommandState, undefined);
      expect(mocks.rpcClient.use).toBeCalledWith(P.Theme.list);
      expect(mockForUse).toBeCalledWith(expected);

      expect(mocks.dispatcher.dispatch).toBeCalledWith(actions.updateList([]));
    });
  });
});
