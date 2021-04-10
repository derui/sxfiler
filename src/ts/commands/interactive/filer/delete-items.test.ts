import * as C from "./delete-items";
import * as M from "@/commands/client-resolver-mock";
import { emptyState } from "@/modules";
import * as FilerModule from "@/modules/filer";
import {
  Filer,
  FileWindow,
  FileList,
  FileItem,
  Target,
  DeleteRequest,
  Side,
  DeleteResponse,
  DeleteResult,
} from "@/generated/filer_pb";
import { also } from "@/libs/fn";
import * as Procs from "@/rpc/client-procedures";
import { actions as logEventActions, LogEventCreators } from "@/modules/log-event";

describe("Commands", () => {
  describe("interactive:filer:Delete Items", () => {
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
      executor.mockImplementation(() =>
        also(new DeleteResponse(), (v) => {
          v.setResult(
            also(new DeleteResult(), (v) => {
              v.setPath("/foo");
              v.setTimestamp("2020-05-20T10:00:00.000Z");
            })
          );
        })
      );
      mocks.rpcClient.use.mockImplementation(() => executor);
      await command.execute(mocks.dispatcher, { clientResolver: mocks.clientResolver, state }, undefined);
      expect(mocks.rpcClient.use).toBeCalledWith(Procs.Filer.deleteItems);

      const expected = also(new DeleteRequest(), (v) => {
        v.setTarget(Target.ONE);
        v.setSide(Side.LEFT);
        v.setTargetId("selected");
      });
      expect(executor).toBeCalledWith(expected);
      expect(mocks.dispatcher.dispatch).toBeCalledWith(
        logEventActions.send([LogEventCreators.createDeleteItem(new Date("2020-05-20T10:00:00.000Z"), "/foo")])
      );
    });
  });
});
