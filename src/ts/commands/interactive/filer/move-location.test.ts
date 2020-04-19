import * as C from "./move-location";
import * as M from "@/commands/client-resolver-mock";
import { CommandState } from "@/commands/type";
import * as P from "@/rpc/client-procedures";
import { Filer, FileWindow, FileList, FileItem, MoveLocationRequest, Side, FileStat } from "@/generated/filer_pb";
import { also } from "@/libs/fn";
import { reducer, actions } from "@/modules/filer";

const createDirectoryItem = function createDirectoryItem(id: string, path: string) {
  const item = new FileItem();
  item.setId(id);
  item.setFullPath(path);
  item.setStat(
    also(new FileStat(), (v) => {
      v.setIsDirectory(true);
      v.setIsFile(false);
      v.setIsSymlink(false);
    })
  );

  return item;
};

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
  describe("interactive:filer:Move Location", () => {
    const filer$ = also(new Filer(), (v) => {
      v.setLeftFileWindow(
        also(new FileWindow(), (v) => {
          v.setFileList(
            also(new FileList(), (v) => {
              v.setItemsList([createDirectoryItem("id1", "/root/foo")]);
            })
          );
        })
      );
    });

    test("call RPC to move location of current side", async () => {
      const mocks = M.createResolverMocks();
      const mockForUse = jest.fn();
      mocks.rpcClient.use.mockImplementation(() => mockForUse);

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
      const expected = new MoveLocationRequest();
      expected.setSide(Side.LEFT);
      expected.setLocation("/root/foo");

      expect(mocks.rpcClient.use).toBeCalledWith(P.Filer.moveLocation);
      expect(mockForUse).toBeCalledWith(expected);
    });

    test("do not call RPC when focus no any item", async () => {
      const mocks = M.createResolverMocks();
      const mockForUse = jest.fn();
      mocks.rpcClient.use.mockImplementation(() => mockForUse);
      const filer$ = also(new Filer(), (v) => {
        v.setLeftFileWindow(
          also(new FileWindow(), (v) => {
            v.setFileList(
              also(new FileList(), (v) => {
                v.setItemsList([]);
              })
            );
          })
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
      expect(mocks.rpcClient.use).not.toBeCalled();
    });

    test("do not call RPC when item focused is not directory", async () => {
      const mocks = M.createResolverMocks();
      const mockForUse = jest.fn();
      mocks.rpcClient.use.mockImplementation(() => mockForUse);
      const filer$ = also(new Filer(), (v) => {
        v.setLeftFileWindow(
          also(new FileWindow(), (v) => {
            v.setFileList(
              also(new FileList(), (v) => {
                v.setItemsList([createFileItem("id1", "/root/foo")]);
              })
            );
          })
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
      expect(mocks.rpcClient.use).not.toBeCalled();
    });
  });
});
