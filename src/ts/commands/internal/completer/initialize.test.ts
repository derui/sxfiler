import * as C from "./initialize";
import * as M from "@/commands/client-resolver-mock";
import { CommandState } from "@/commands/type";
import * as P from "@/rpc/client-procedures";
import { InitializeRequest, Item } from "@/generated/completer_pb";
import { also } from "@/libs/fn";

describe("Commands", () => {
  describe("internal:completer:Initialize", () => {
    test("call complete RPC to complete with input", async () => {
      const mocks = M.createResolverMocks();
      const mockForUse = jest.fn();
      mockForUse.mockImplementation(() => Promise.resolve());
      mocks.rpcClient.use.mockImplementation(() => mockForUse);

      const source = [
        also(new Item(), (v) => {
          v.setId("id");
          v.setValue("foo");
        }),
        also(new Item(), (v) => {
          v.setId("id2");
          v.setValue("bar");
        }),
      ];
      const command = C.createCommand();
      await command.execute(mocks.dispatcher, { clientResolver: mocks.clientResolver } as CommandState, {
        source,
      });

      expect(mocks.rpcClient.use).toBeCalledWith(P.Completer.initialize);

      const expected = new InitializeRequest();
      expected.setSourceList(source);
      expect(mockForUse).toBeCalledWith(expected);
    });
  });
});
