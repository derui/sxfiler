import * as C from "./filer-toggle-mark";
import * as AppState from "../../states";
import { Side } from "../../states/file-list";
import { createFiler } from "../../domains/filer";
import { Apis } from "../../apis";
import { actions } from "../../actions/filer";
import { createNode } from "../../domains/node";
import { createFileStat } from "../../domains/file-stat";
import { emptyMode } from "../../domains/mode";
import * as FileListState from "../../states/file-list";

const nodes = [
  createNode({
    id: "node1",
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
        const filer = createFiler({ id: "id", nodes, location: "test", currentCursorIndex: 0 });
        client.call.mockResolvedValue(filer);

        const state = AppState.empty();
        state.fileList = FileListState.initialize(state.fileList, { left: filer, right: filer });

        await command.execute(dispatcher as any, { state, client: client as any });
        expect(client.call).toBeCalledWith(Apis.Filer.ToggleMark, { name: Side.Left, nodeIds: [nodes[0].id] });
      });

      it("update a filer after to toggle mark of the node", async () => {
        const command = C.createCommand();
        const dispatcher = {
          dispatch: jest.fn(),
        };
        const client = {
          call: jest.fn(),
        };
        const filer = createFiler({ id: "id", nodes, location: "test", currentCursorIndex: 0 });
        const updatedFiler = createFiler({ id: "id", nodes: [], location: "test", currentCursorIndex: 0 });
        const state = AppState.empty();
        state.fileList = FileListState.initialize(state.fileList, { left: filer, right: filer });

        client.call.mockResolvedValue(updatedFiler);

        await command.execute(dispatcher as any, { state, client: client as any });
        await expect(dispatcher.dispatch).toBeCalledWith(
          actions.update({
            side: Side.Left,
            filer: updatedFiler,
          })
        );
      });
    });
  });
});
