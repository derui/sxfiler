import * as C from "./open";
import * as Procs from "@/rpc/client-procedures";
import * as M from "@/commands/client-resolver-mock";
import { emptyState } from "@/modules";
import * as F from "@/modules/filer";
import * as Completer from "@/modules/completer";
import * as Keymap from "@/modules/keymap";
import { Filer, FileWindow, FileList, FileItem } from "@/generated/filer_pb";
import { InitializeRequest, Item, Candidate } from "@/generated/completer_pb";
import { also } from "@/libs/fn";
import { UIContext } from "@/types/ui-context";

describe("Commands", () => {
  describe("interactive:finder:Open", () => {
    test("call RPC and dispatch action", async () => {
      const filer = also(new Filer(), (v) => {
        v.setLeftFileWindow(
          also(new FileWindow(), (v) => {
            v.setFileList(
              also(new FileList(), (v) => {
                v.setItemsList([
                  also(new FileItem(), (v) => {
                    v.setId("item");
                    v.setName("name");
                  }),
                  also(new FileItem(), (v) => {
                    v.setId("item2");
                    v.setName("name2");
                  }),
                ]);
              })
            );
          })
        );
      });
      const state = { ...emptyState, filer: F.reducer(undefined, F.actions.update(filer)) };

      const mocks = M.createResolverMocks();
      const command = C.createCommand();
      const use = jest.fn();
      mocks.rpcClient.use.mockImplementation(() => use);

      await command.execute(mocks.dispatcher, { clientResolver: mocks.clientResolver, state }, undefined);

      expect(mocks.rpcClient.use).toBeCalledWith(Procs.Completer.initialize);
      expect(mocks.dispatcher.dispatch).toBeCalledWith(Completer.actions.open("Find items"));
      expect(mocks.dispatcher.dispatch).toBeCalledWith(
        Keymap.actions.replaceContext([UIContext.OnCompletion, UIContext.ForFinder])
      );

      const request = new InitializeRequest();
      const source = [
        also(new Item(), (v) => {
          v.setId("item");
          v.setValue("name");
        }),
        also(new Item(), (v) => {
          v.setId("item2");
          v.setValue("name2");
        }),
      ];
      request.setSourceList(source);
      expect(use).toBeCalledWith(request);

      const candidates = source.map((v) => {
        const c = new Candidate();
        c.setStart(0);
        c.setLength(0);
        c.setValue(v.clone());
        return c;
      });
      expect(mocks.dispatcher.dispatch).toBeCalledWith(Completer.actions.updateCandidates(candidates));
    });
  });
});
