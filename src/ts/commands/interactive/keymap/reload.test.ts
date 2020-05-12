import * as C from "./reload";
import * as M from "@/commands/client-resolver-mock";
import { CommandState } from "@/commands/type";
import * as P from "@/rpc/client-procedures";
import { actions } from "@/modules/keymap";
import { GetResponse, Keymap } from "@/generated/keymap_pb";

describe("Commands", () => {
  describe("interactive:keymap:Get", () => {
    test("call RPC to get keymap", async () => {
      const mocks = M.createResolverMocks();
      const mockForUse = jest.fn();
      mocks.rpcClient.use.mockImplementation(() => mockForUse);

      const response = new GetResponse();
      const keymap = new Keymap();
      response.setKeymap(keymap);
      mockForUse.mockImplementation(() => response);

      const command = C.createCommand();
      await command.execute(mocks.dispatcher, { clientResolver: mocks.clientResolver } as CommandState, undefined);

      expect(mocks.rpcClient.use).toBeCalledWith(P.Keymap.getKeymap);
      expect(mocks.dispatcher.dispatch).toBeCalledWith(actions.update(keymap));
    });
  });
});
