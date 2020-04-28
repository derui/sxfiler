import * as C from "./move";
import * as M from "@/commands/client-resolver-mock";
import { emptyState } from "@/modules";
import * as FilerModule from "@/modules/filer";
import { Filer, FileWindow, FileList, FileItem, MoveRequest, Target, Direction } from "@/generated/filer_pb";
import { also } from "@/libs/fn";
import * as Procs from "@/rpc/client-procedures";

describe("Commands", () => {
  describe("interactive:filer:Move", () => {
    test("call RPC to move when have no any marked items", async () => {
      const command = C.createCommand();
      const mocks = M.createResolverMocks();

      const filer = also(new Filer(), (v) => {
        v.setLeftFileWindow(
          also(new FileWindow(), (v) => {
            v.setFileList(
              also(new FileList(), (v) => {
                v.addItems(
                  also(new FileItem(), (v) => {
                    v.setId("selected");
                  })
                );
                v.addItems(
                  also(new FileItem(), (v) => {
                    v.setId("not selected");
                  })
                );
              })
            );
          })
        );
      });
      const F = FilerModule;
      const state = {
        ...emptyState,
        filer: F.reducer(F.emptyState, F.actions.update(filer)),
      };

      const executor = jest.fn();
      mocks.rpcClient.use.mockImplementation(() => executor);
      await command.execute(mocks.dispatcher, { clientResolver: mocks.clientResolver, state }, undefined);
      expect(mocks.rpcClient.use).toBeCalledWith(Procs.Filer.move);

      const expected = also(new MoveRequest(), (v) => {
        v.setTarget(Target.ONE);
        v.setDirection(Direction.LEFT_TO_RIGHT);
        v.setTargetId("selected");
      });
      expect(executor).toBeCalledWith(expected);
    });
  });
});
