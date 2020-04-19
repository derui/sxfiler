import * as C from "./initialize";
import * as M from "@/commands/client-resolver-mock";
import { CommandState } from "@/commands/type";
import * as P from "@/rpc/client-procedures";
import { InitializeRequest } from "@/generated/filer_pb";

describe("Commands", () => {
  describe("internal:filer:Initialize", () => {
    test("call RPC to initialize filer ", async () => {
      const mocks = M.createResolverMocks();
      const mockForUse = jest.fn();
      mocks.rpcClient.use.mockImplementation(() => mockForUse);

      const command = C.createCommand();
      await command.execute(mocks.dispatcher, { clientResolver: mocks.clientResolver } as CommandState, {
        leftLocation: "left",
        rightLocation: "right",
      });

      expect(mocks.rpcClient.use).toBeCalledWith(P.Filer.initialize);

      const expected = new InitializeRequest();
      expected.setLeftLocation("left");
      expected.setRightLocation("right");
      expect(mockForUse).toBeCalledWith(expected);
    });
  });
});
