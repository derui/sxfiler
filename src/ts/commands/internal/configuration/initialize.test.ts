import * as C from "./initialize";
import * as M from "@/commands/client-resolver-mock";
import { CommandState } from "@/commands/type";
import { GetRequest, Configuration, GetResponse } from "@/generated/configuration_pb";
import * as P from "@/rpc/client-procedures";
import { actions } from "@/modules/configuration";

describe("Commands", () => {
  describe("internal:configuration:Initialize", () => {
    test("call rpc to get configuration", async () => {
      const config = new Configuration();
      const mocks = M.createResolverMocks();
      const mockForUse = jest.fn();
      mocks.rpcClient.use.mockImplementation(() => mockForUse);
      mockForUse.mockImplementation(() => {
        const res = new GetResponse();
        res.setConfiguration(config);
        return res;
      });

      const command = C.createCommand();
      await command.execute(mocks.dispatcher, { clientResolver: mocks.clientResolver } as CommandState, undefined);

      expect(mocks.rpcClient.use).toBeCalledWith(P.Configuration.getConfiguration);

      const expected = new GetRequest();
      expect(mockForUse).toBeCalledWith(expected);
      expect(mocks.dispatcher.dispatch).toBeCalledWith(actions.update(config));
    });
  });
});
