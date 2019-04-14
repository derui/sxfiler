import * as C from "./filer-move-cursor-up";
import * as AppState from "../../states";
import { Side } from "../../states/file-list";
import { createFiler, Direction } from "../../domains/filer";

describe("Commands", () => {
  describe("Filer", () => {
    describe("Move the cursor up of the filer", () => {
      it("throw error when pass undefined as argument", () => {
        const command = C.createCommand();
        const dispatcher = jest.fn();

        expect(() => command.execute(dispatcher as any, undefined)).toThrowError();
      });

      it("do not call anything when filer is not initialized", () => {
        const command = C.createCommand();
        const dispatcher = {
          dispatch: jest.fn(),
        };
        const state = AppState.empty();
        command.execute(dispatcher as any, { state: state, client: jest.fn() as any });
        expect(dispatcher.dispatch).not.toBeCalled();
      });

      it("call dispatch function with argument when filer initialized", () => {
        const command = C.createCommand();
        const dispatcher = {
          dispatch: jest.fn(),
        };
        const state = AppState.empty();
        state.fileList.initialized = true;
        state.fileList.currentSide = Side.Left;
        state.fileList.left = createFiler({ id: "id", nodes: [], location: "test", currentCursorIndex: 0 });

        command.execute(dispatcher as any, { state: state, client: jest.fn() as any });
        expect(dispatcher.dispatch).toBeCalled();
      });

      it("moves filer cursor up", () => {
        const command = C.createCommand();
        const dispatcher = {
          dispatch: jest.fn(),
        };
        const state = AppState.empty();
        state.fileList.initialized = true;
        state.fileList.currentSide = Side.Left;
        state.fileList.left = createFiler({ id: "id", nodes: [], location: "test", currentCursorIndex: 0 });
        const spy = jest.spyOn(state.fileList.left, "moveIndex");

        command.execute(dispatcher as any, { state: state, client: jest.fn() as any });
        expect(spy).toBeCalledWith(Direction.Up);
      });
    });
  });
});
