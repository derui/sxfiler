import * as C from "./filer-move-items";
import * as AppState from "../../states";
import { Side } from "../../states/file-list";
import { createFiler } from "../../domains/filer";
import { Apis } from "../../apis";
import { actions } from "../../actions/filer";
import { createFileItem } from "../../domains/file-item";
import { createFileStat } from "../../domains/file-stat";
import * as FileListState from "../../states/file-list";
import { emptyMode } from "../../domains/mode";

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
  describe("Filer", () => {
    describe("Move nodes", () => {
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
        const left = createFiler({ id: "id", name: "name", items, location: "test", currentCursorIndex: 0 });
        const right = createFiler({ id: "id", name: "name", items, location: "test", currentCursorIndex: 0 });
        client.call.mockImplementation((typ, arg) => {
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

        await command.execute(dispatcher as any, { state, client: client as any });
        expect(client.call).toBeCalledWith(Apis.Filer.Move, {
          source: Side.Left,
          dest: Side.Right,
          nodeIds: [items[0].id],
        });
      });

      it("dispatch reload action to update each side", async () => {
        const command = C.createCommand();
        const dispatcher = {
          dispatch: jest.fn(),
        };
        const client = {
          call: jest.fn(),
        };
        const left = createFiler({ id: "id", name: "name", items, location: "test", currentCursorIndex: 0 });
        const right = createFiler({ id: "id", name: "name", items, location: "test", currentCursorIndex: 0 });
        client.call.mockImplementation((typ, arg) => {
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

        await command.execute(dispatcher as any, { state, client: client as any });
        expect(dispatcher.dispatch).toBeCalledWith(actions.reload({ filers: [left, right] }));
      });
    });
  });
});
