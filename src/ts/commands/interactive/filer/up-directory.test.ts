import * as C from "./up-directory";
import * as M from "@/commands/client-resolver-mock";
import { emptyState } from "@/modules";
import * as FilerModule from "@/modules/filer";
import { Filer, FileWindow, FileList, FileItem, Side, UpDirectoryRequest } from "@/generated/filer_pb";
import { also } from "@/libs/fn";
import * as procs from "@/rpc/client-procedures";

describe("Commands", () => {
  describe("interactive:filer:Up Directory", () => {
    test("call API when selected some item", async () => {
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
      expect(mocks.rpcClient.use).toBeCalledWith(procs.Filer.upDirectory);

      const expected = also(new UpDirectoryRequest(), (v) => {
        v.setSide(Side.LEFT);
      });
      expect(executor).toBeCalledWith(expected);
    });
  });
});
