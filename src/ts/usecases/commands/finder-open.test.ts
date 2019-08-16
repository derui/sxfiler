import * as C from "./finder-open";
import * as AppState from "@/states";
import { Side } from "@/states/file-list";
import { createFiler } from "@/domains/filer";
import { Apis } from "@/apis";
import * as actions from "@/actions/finder";
import { createFileItem } from "@/domains/file-item";
import { createFileStat } from "@/domains/file-stat";
import { emptyMode } from "@/domains/mode";
import * as FileListState from "@/states/file-list";
import { createLocationHistory } from "@/domains/location-history";
import { createCandidate } from "@/domains/candidate";

const items = [
  createFileItem({
    id: "node1",
    name: "node",
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
  const history = createLocationHistory({ records: [], maxRecordNumber: 100 });

  describe("Filer", () => {
    describe("Toggle mark of current focusing node in the filer", () => {
      it("throw error when pass undefined as argument", async () => {
        const command = C.createCommand();
        const dispatcher = jest.fn();

        await expect(command.execute(dispatcher as any, undefined)).rejects.toThrowError();
      });

      it("call API to move location of a filer", async () => {
        const command = C.createCommand();
        const dispatcher = {
          dispatch: jest.fn(),
        };
        const client = {
          call: jest.fn(),
        };
        const filer = createFiler({ id: "id", name: "name", items, location: "test", currentCursorIndex: 0, history });
        client.call.mockResolvedValue(filer);

        const state = AppState.empty();
        state.fileList = FileListState.initialize(state.fileList, { left: filer, right: filer });

        await command.execute(dispatcher as any, { state, client: client as any });
        expect(client.call).toBeCalledWith(Apis.Completion.Setup, {
          source: items.map(v => createCandidate({ id: v.id, value: v.name })),
        });
      });

      it("update a filer after to toggle mark of the node", async () => {
        const command = C.createCommand();
        const dispatcher = {
          dispatch: jest.fn(),
        };
        const client = {
          call: jest.fn(),
        };
        const filer = createFiler({ id: "id", name: "name", items, location: "test", currentCursorIndex: 0, history });
        const updatedFiler = createFiler({
          id: "id",
          name: "name",
          items: [],
          location: "test",
          currentCursorIndex: 0,
          history,
        });
        const state = AppState.empty();
        state.fileList = FileListState.initialize(state.fileList, { left: filer, right: filer });

        client.call.mockResolvedValue(updatedFiler);

        await command.execute(dispatcher as any, { state, client: client as any });
        await expect(dispatcher.dispatch).toBeCalledWith(actions.open(Side.Left));
      });
    });
  });
});
