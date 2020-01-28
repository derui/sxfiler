import * as C from "./reload-all";
import * as M from "@/commands/client-resolver-mock";
import { CommandState } from "@/commands/type";
import * as P from "@/rpc/client-procedures";
import { ReloadAllRequest } from "@/generated/filer_pb";

describe("Commands", () => {
  describe("interactive:filer:Reload All", () => {
    test("call RPC to reload filer ", async () => {
      const mocks = M.createResolverMocks();
      const mockForUse = jest.fn();
      mocks.rpcClient.use.mockImplementation(() => mockForUse);

      const command = C.createCommand();
      await command.execute(mocks.dispatcher, { clientResolver: mocks.clientResolver } as CommandState, undefined);

      expect(mocks.rpcClient.use).toBeCalledWith(P.Filer.reloadAll);

      const expected = new ReloadAllRequest();
      expect(mockForUse).toBeCalledWith(expected);
    });
  });
});
