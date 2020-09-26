import * as M from "@/commands/client-resolver-mock";
import { CommandState } from "@/commands/type";
import { actions, reducer } from "@/modules/filer";
import * as C from "./open-item";
import { Filer, FileWindow, FileItem, FileStat, FileList } from "@/generated/filer_pb";
import { also } from "@/libs/fn";

const createFileItem = function createFileItem(id: string, path: string) {
  const item = new FileItem();
  item.setId(id);
  item.setFullPath(path);
  item.setStat(
    also(new FileStat(), (v) => {
      v.setIsDirectory(false);
      v.setIsFile(true);
      v.setIsSymlink(false);
    })
  );

  return item;
};

describe("Commands", () => {
  describe("interactive:filer:Open Item", () => {
    test("call API in app client", async () => {
      const mocks = M.createResolverMocks();
      const item = createFileItem("id1", "/root/foo");
      const filer$ = also(new Filer(), (v) => {
        v.setLeftFileWindow(
          also(new FileWindow(), (v) =>
            v.setFileList(
              also(new FileList(), (v) => {
                v.setItemsList([item]);
              })
            )
          )
        );
      });

      const filer = reducer(undefined, actions.update(filer$));

      // do
      await C.createCommand().execute(
        mocks.dispatcher,
        {
          clientResolver: mocks.clientResolver,
          state: {
            filer,
          },
        } as CommandState,
        undefined
      );

      // verify

      expect(mocks.appClient.openItem).toBeCalledWith(item);
    });
  });
});
