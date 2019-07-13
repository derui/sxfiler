import * as C from "./filer-move-cursor-up";
import * as AppState from "@/states";
import { Side } from "@/states/file-list";
import { createFiler, Direction } from "@/domains/filer";

describe("Commands", () => {
  describe("Filer", () => {
    describe("Move the cursor up of the filer", () => {
      it("throw error when pass undefined as argument", async () => {
        const command = C.createCommand();
        const dispatcher = jest.fn();

        await expect(command.execute(dispatcher as any, undefined)).rejects.toThrowError();
      });

      it("do not call anything when filer is not initialized", async () => {
        const command = C.createCommand();
        const dispatcher = {
          dispatch: jest.fn(),
        };
        const state = AppState.empty();
        await command.execute(dispatcher as any, { state: state, client: jest.fn() as any });
        expect(dispatcher.dispatch).not.toBeCalled();
      });

      it("call dispatch function with argument when filer initialized", async () => {
        const command = C.createCommand();
        const dispatcher = {
          dispatch: jest.fn(),
        };
        const state = AppState.empty();
        state.fileList.currentSide = Side.Left;
        state.fileList.left = createFiler({
          id: "id",
          name: "name",
          items: [],
          location: "test",
          currentCursorIndex: 0,
        });

        await command.execute(dispatcher as any, { state: state, client: jest.fn() as any });
        expect(dispatcher.dispatch).toBeCalled();
      });

      it("moves filer cursor up", async () => {
        const command = C.createCommand();
        const dispatcher = {
          dispatch: jest.fn(),
        };
        const state = AppState.empty();
        state.fileList.currentSide = Side.Left;
        state.fileList.left = createFiler({
          id: "id",
          name: "name",
          items: [],
          location: "test",
          currentCursorIndex: 0,
        });
        const spy = jest.spyOn(state.fileList.left, "moveIndex");

        await command.execute(dispatcher as any, { state: state, client: jest.fn() as any });
        expect(spy).toBeCalledWith(Direction.Up);
      });
    });
  });
});
