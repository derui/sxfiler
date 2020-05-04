import * as C from "./copy";
import * as M from "@/commands/client-resolver-mock";
import { emptyState } from "@/modules";
import * as FilerModule from "@/modules/filer";
import { Filer, FileWindow, FileList, FileItem, CopyRequest, Target, Direction, Transfer } from "@/generated/filer_pb";
import { also } from "@/libs/fn";
import * as Procs from "@/rpc/client-procedures";

describe("Commands", () => {
  describe("interactive:filer:Copy", () => {
    test("call RPC to copy when have no any marked items", async () => {
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
      expect(mocks.rpcClient.use).toBeCalledWith(Procs.Filer.copy);

      const expected = also(new CopyRequest(), (v) => {
        const transfer = also(new Transfer(), (v) => {
          v.setTarget(Target.ONE);
          v.setDirection(Direction.LEFT_TO_RIGHT);
          v.setTargetId("selected");
        });
        v.setTransfer(transfer);
      });
      expect(executor).toBeCalledWith(expected);
    });
  });
});
