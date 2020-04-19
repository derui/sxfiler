import * as C from "./complete";
import * as M from "@/commands/client-resolver-mock";
import { CommandState } from "@/commands/type";
import * as P from "@/rpc/client-procedures";
import { CompleteRequest } from "@/generated/completer_pb";

describe("Commands", () => {
  describe("internal:completer:Complete", () => {
    test("call complete RPC to complete with input", async () => {
      const mocks = M.createResolverMocks();
      const mockForUse = jest.fn();
      mockForUse.mockImplementation(() => Promise.resolve());
      mocks.rpcClient.use.mockImplementation(() => mockForUse);

      const command = C.createCommand();
      await command.execute(mocks.dispatcher, { clientResolver: mocks.clientResolver } as CommandState, {
        input: "foo",
      });

      expect(mocks.rpcClient.use).toBeCalledWith(P.Completer.complete);

      const expected = new CompleteRequest();
      expected.setInput("foo");
      expect(mockForUse).toBeCalledWith(expected);
    });
  });
});
