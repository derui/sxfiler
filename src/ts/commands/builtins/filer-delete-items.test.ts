import * as C from "./filer-delete-items";
import * as AppState from "@/states";
import { Side } from "@/states/file-list";
import { createFiler } from "@/domains/filer";
import { Apis } from "@/apis";
import { createFileItem } from "@/domains/file-item";
import { createFileStat } from "@/domains/file-stat";
import * as FileListState from "@/states/file-list";
import { emptyMode } from "@/domains/mode";
import { createLocationHistory } from "@/domains/location-history";
import { createResolverMocks } from "../client-resolver-mock";

const items = [
  createFileItem({
    id: "node1",
    name: "node",
    fullPath: "/parent/node",
    parentDirectory: "/parent",
    marked: false,
    stat: createFileStat({
      mode: emptyMode(),
      uid: 1000,
      gid: 1000,
      atime: "1",
      ctime: "2",
      mtime: "3",
      size: "10",
      isDirectory: false,
      isFile: true,
      isSymlink: false,
    }),
  }),
];

describe("Commands", () => {
  describe("Filer", () => {
    describe("Delete items", () => {
      const history = createLocationHistory({ records: [], maxRecordNumber: 100 });
      const { dispatcher, apiClient, clientResolver } = createResolverMocks();

      afterEach(jest.clearAllMocks);

      it("throw error when pass undefined as argument", async () => {
        const command = C.createCommand();

        await expect(command.execute(dispatcher, undefined)).rejects.toThrowError();
      });

      it("call API to delete items in the filer", async () => {
        const command = C.createCommand();
        const left = createFiler({ id: "id", name: "name", items, location: "test", currentCursorIndex: 0, history });
        const right = createFiler({ id: "id", name: "name", items, location: "test", currentCursorIndex: 0, history });
        apiClient.call.mockImplementation((typ, arg) => {
          if (typ === Apis.Filer.Move) {
            return Promise.resolve();
          }

          switch (arg) {
            case Side.Left:
              return left;
            case Side.Right:
              return right;
          }
        });

        const state = AppState.empty();
        state.fileList = FileListState.initialize(state.fileList, { left, right });

        await command.execute(dispatcher, { state, clientResolver });
        expect(apiClient.call).toBeCalledWith(Apis.Filer.Delete, {
          source: Side.Left,
          itemIds: [items[0].id],
        });
      });
    });
  });
});
