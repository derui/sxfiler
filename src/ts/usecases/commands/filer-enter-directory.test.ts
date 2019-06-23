import * as C from "./filer-enter-directory";
import * as AppState from "../../states";
import { Side, initialize } from "../../states/file-list";
import { createFiler } from "../../domains/filer";
import { Apis } from "../../apis";
import { actions } from "../../actions/filer";
import { createFileItem } from "../../domains/file-item";
import { emptyMode } from "../../domains/mode";
import { createFileStat } from "../../domains/file-stat";

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
      it("throw error when pass undefined as argument", async () => {
        const command = C.createCommand();
        const dispatcher = jest.fn();

        await expect(command.execute(dispatcher as any, undefined)).rejects.toThrowError();
      });

      it("call API to enter into the directory", async () => {
        const command = C.createCommand();
        const dispatcher = {
          dispatch: jest.fn(),
        };
        const client = {
          call: jest.fn(),
        };
        client.call.mockResolvedValue(
          createFiler({ id: "id1", name: "name", items: [], location: "entered", currentCursorIndex: 0 })
        );
        const state = AppState.empty();
        state.fileList = initialize(state.fileList, {
          left: createFiler({
            id: "id1",
            name: "name",
            items: [
              createFileItem({
                id: "node",
                parentDirectory: "parent",
                name: "dir",
                stat,
                marked: false,
              }),
            ],
            location: "test",
            currentCursorIndex: 0,
          }),
          right: createFiler({ id: "id2", name: "name", items: [], location: "test", currentCursorIndex: 0 }),
        });

        await command.execute(dispatcher as any, { state, client: client as any });

        expect(client.call).toBeCalledWith(Apis.Filer.EnterDirectory, { name: Side.Left, nodeId: "node" });
      });

      it("update a filer after entered into directory", async () => {
        const command = C.createCommand();
        const dispatcher = {
          dispatch: jest.fn(),
        };
        const client = {
          call: jest.fn(),
        };
        const state = AppState.empty();
        state.fileList = initialize(state.fileList, {
          left: createFiler({
            id: "id1",
            name: "name",
            items: [
              createFileItem({
                id: "node",
                name: "node",
                parentDirectory: "test",
                stat,
                marked: false,
              }),
            ],
            location: "test",
            currentCursorIndex: 0,
          }),
          right: createFiler({ id: "id2", name: "name", items: [], location: "test", currentCursorIndex: 0 }),
        });

        const filer = createFiler({ id: "id1", name: "name", items: [], location: "test/node", currentCursorIndex: 0 });
        client.call.mockResolvedValue(filer);

        await command.execute(dispatcher as any, { state, client: client as any });

        await expect(dispatcher.dispatch).toBeCalledWith(actions.load({ filer }));
      });
    });
  });
});
