import * as C from "./update";
import * as M from "@/commands/client-resolver-mock";
import { CommandState } from "@/commands/type";
import * as P from "@/rpc/client-procedures";
import { itemKeys } from "@/configurations";
import { UpdateRequest } from "@/generated/configuration_pb";

describe("Commands", () => {
  describe("internal:configuration:Update", () => {
    test("call configuration RPC to update value of the key", async () => {
      const mocks = M.createResolverMocks();
      const mockForUse = jest.fn();
      mockForUse.mockImplementation(() => Promise.resolve());
      mocks.rpcClient.use.mockImplementation(() => mockForUse);

      const command = C.createCommand();
      await command.execute(mocks.dispatcher, { clientResolver: mocks.clientResolver } as CommandState, {
        itemKey: itemKeys.general.behaviors.confirmationWhenDelete,
        value: false,
      });

      expect(mocks.rpcClient.use).toBeCalledWith(P.Configuration.updateConfiguration);

      const expected = new UpdateRequest();
      expected.setKeyList(Array.from(itemKeys.general.behaviors.confirmationWhenDelete));
      expected.setJsonValue(JSON.stringify(false));
      expect(mockForUse).toBeCalledWith(expected);
    });
  });
});
