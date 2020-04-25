import * as C from "./confirm";
import * as M from "@/commands/client-resolver-mock";
import { emptyState } from "@/modules";
import * as F from "@/modules/filer";
import * as Completer from "@/modules/completer";
import * as Keymap from "@/modules/keymap";
import * as FilerModule from "@/modules/filer";
import { Filer, FileWindow, FileList, FileItem } from "@/generated/filer_pb";
import { Item, Candidate } from "@/generated/completer_pb";
import { also, pipe } from "@/libs/fn";
import { UIContext } from "@/types/ui-context";

describe("Commands", () => {
  describe("interactive:finder:Confirm", () => {
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

      const state = {
        ...emptyState,
        filer: F.reducer(undefined, F.actions.update(filer)),
        completer: pipe(
          (v) => Completer.reducer(v, Completer.actions.open("foo")),
          (v) =>
            Completer.reducer(
              v,
              Completer.actions.updateCandidates([
                also(new Candidate(), (v) => {
                  v.setValue(
                    also(new Item(), (v) => {
                      v.setId("item");
                    })
                  );
                }),
              ])
            )
        )(undefined),
      };

      const mocks = M.createResolverMocks();
      const command = C.createCommand();
      const use = jest.fn();
      mocks.rpcClient.use.mockImplementation(() => use);

      await command.execute(mocks.dispatcher, { clientResolver: mocks.clientResolver, state }, undefined);

      expect(mocks.dispatcher.dispatch).toBeCalledWith(Completer.actions.close());
      expect(mocks.dispatcher.dispatch).toBeCalledWith(Keymap.actions.replaceContext([UIContext.OnFileTree]));
      expect(mocks.dispatcher.dispatch).toBeCalledWith(FilerModule.actions.focusItem("item"));
    });
  });
});
