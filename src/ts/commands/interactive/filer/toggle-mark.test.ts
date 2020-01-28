import * as C from "./toggle-mark";
import * as M from "@/commands/client-resolver-mock";
import { emptyState } from "@/modules";
import * as FilerModule from "@/modules/filer";
import { Filer, FileWindow, FileList, FileItem, Side, ToggleMarkOfItemRequest } from "@/generated/filer_pb";
import { also } from "@/libs/fn";
import * as procs from "@/rpc/client-procedures";

describe("Commands", () => {
  describe("interactive:filer:Toggle Mark", () => {
    test("call RPC for item", async () => {
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
      expect(mocks.rpcClient.use).toBeCalledWith(procs.Filer.toggleMark);

      const expected = also(new ToggleMarkOfItemRequest(), (v) => {
        v.setSide(Side.LEFT);
        v.setItemId("selected");
      });
      expect(executor).toBeCalledWith(expected);
    });
  });
});
