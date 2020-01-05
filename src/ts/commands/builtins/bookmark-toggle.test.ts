import * as C from "./bookmark-toggle";
import * as AppState from "@/states";
import { Apis } from "@/apis";
import * as actions from "@/actions/bookmark";
import { createBookmark } from "@/domains/bookmark";
import { initialize, updateBookmarks } from "@/states/file-list";
import { createFileItem } from "@/domains/file-item";
import { createFileStat } from "@/domains/file-stat";
import { emptyMode } from "@/domains/mode";
import { createFiler } from "@/domains/filer";
import { createLocationHistory } from "@/domains/location-history";
import { createResolverMocks } from "../client-resolver-mock";

const bookmark = createBookmark({ id: "1", path: "/parent/node", order: 1 });

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
const history = createLocationHistory({ records: [], maxRecordNumber: 100 });
const left = createFiler({ id: "id", name: "name", items, location: "test", currentCursorIndex: 0, history });
const right = createFiler({ id: "id", name: "name", items, location: "test", currentCursorIndex: 0, history });

describe("Commands", () => {
  describe("Bookmark", () => {
    const { dispatcher, apiClient, clientResolver } = createResolverMocks();

    afterEach(() => jest.clearAllMocks());

    describe("register new bookmark", () => {
      it("throw error when pass undefined as argument", async () => {
        const command = C.createCommand();

        await expect(command.execute(dispatcher, undefined)).rejects.toThrowError();
      });

      it("call API to register bookmark on the server", async () => {
        const command = C.createCommand();
        const state = AppState.empty();

        apiClient.call.mockResolvedValue(bookmark);
        state.fileList = initialize(state.fileList, { left, right });

        await command.execute(dispatcher, { state, clientResolver });
        expect(apiClient.call).toBeCalledWith(Apis.Bookmark.Register, "/parent/node");
      });

      it("dispatch an action to register bookmark", async () => {
        const command = C.createCommand();
        const state = AppState.empty();

        apiClient.call.mockResolvedValue(bookmark);
        state.fileList = initialize(state.fileList, { left, right });

        await command.execute(dispatcher, { state, clientResolver });
        expect(dispatcher.dispatch).toBeCalledWith(actions.registerBookmark(bookmark));
      });

      it("call API to delete bookmark on the server", async () => {
        const command = C.createCommand();
        const state = AppState.empty();
        apiClient.call.mockResolvedValue(bookmark);
        state.fileList = updateBookmarks([bookmark])(initialize(state.fileList, { left, right }));

        await command.execute(dispatcher, { state, clientResolver });
        expect(apiClient.call).toBeCalledWith(Apis.Bookmark.Delete, "1");
      });

      it("dispatch an action to delete bookmark", async () => {
        const command = C.createCommand();
        const state = AppState.empty();

        apiClient.call.mockResolvedValue(bookmark);
        state.fileList = updateBookmarks([bookmark])(initialize(state.fileList, { left, right }));

        await command.execute(dispatcher, { state, clientResolver });
        expect(dispatcher.dispatch).toBeCalledWith(actions.deleteBookmark(bookmark));
      });
    });
  });
});
