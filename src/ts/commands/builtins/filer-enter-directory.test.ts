import * as C from "./filer-enter-directory";
import * as AppState from "@/states";
import { Side, initialize } from "@/states/file-list";
import { createFiler } from "@/domains/filer";
import { Apis } from "@/apis";
import * as actions from "@/actions/filer";
import { createFileItem } from "@/domains/file-item";
import { emptyMode } from "@/domains/mode";
import { createFileStat } from "@/domains/file-stat";
import { createLocationHistory } from "@/domains/location-history";
import { createResolverMocks } from "../client-resolver-mock";

const stat = createFileStat({
  mode: emptyMode(),
  uid: 1000,
  gid: 1000,
  atime: "10",
  ctime: "11",
  mtime: "12",
  size: "100",
  isDirectory: true,
  isFile: false,
  isSymlink: false,
});

describe("Commands", () => {
  describe("Filer", () => {
    describe("Enter into the directory", () => {
      const history = createLocationHistory({ records: [], maxRecordNumber: 100 });
      const { dispatcher, clientResolver, apiClient } = createResolverMocks();

      afterEach(jest.clearAllMocks);

      it("throw error when pass undefined as argument", async () => {
        const command = C.createCommand();

        await expect(command.execute(dispatcher, undefined)).rejects.toThrowError();
      });

      it("call API to enter into the directory", async () => {
        const command = C.createCommand();

        apiClient.call.mockResolvedValue(
          createFiler({ id: "id1", name: "name", items: [], location: "entered", currentCursorIndex: 0, history })
        );
        const state = AppState.empty();
        state.fileList = initialize(state.fileList, {
          left: createFiler({
            id: "id1",
            name: "name",
            items: [
              createFileItem({
                id: "node",
                fullPath: "parent/dir",
                parentDirectory: "parent",
                name: "dir",
                stat,
                marked: false,
              }),
            ],
            location: "test",
            currentCursorIndex: 0,
            history,
          }),
          right: createFiler({ id: "id2", name: "name", items: [], location: "test", currentCursorIndex: 0, history }),
        });

        await command.execute(dispatcher, { state, clientResolver });

        expect(apiClient.call).toBeCalledWith(Apis.Filer.EnterDirectory, { name: Side.Left, itemId: "node" });
      });

      it("update a filer after entered into directory", async () => {
        const command = C.createCommand();
        const state = AppState.empty();
        state.fileList = initialize(state.fileList, {
          left: createFiler({
            id: "id1",
            name: "name",
            items: [
              createFileItem({
                id: "node",
                name: "node",
                fullPath: "test/node",
                parentDirectory: "test",
                stat,
                marked: false,
              }),
            ],
            location: "test",
            currentCursorIndex: 0,
            history,
          }),
          right: createFiler({ id: "id2", name: "name", items: [], location: "test", currentCursorIndex: 0, history }),
        });

        const filer = createFiler({
          id: "id1",
          name: "name",
          items: [],
          location: "test/node",
          currentCursorIndex: 0,
          history,
        });
        apiClient.call.mockResolvedValue(filer);

        await command.execute(dispatcher, { state, clientResolver });

        expect(dispatcher.dispatch).toBeCalledWith(actions.load({ filer }));
      });
    });
  });
});
