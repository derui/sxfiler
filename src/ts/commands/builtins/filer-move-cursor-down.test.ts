import * as C from "./filer-move-cursor-down";
import * as AppState from "@/states";
import { Side } from "@/states/file-list";
import { createFiler, Direction } from "@/domains/filer";
import { createLocationHistory } from "@/domains/location-history";
import { createResolverMocks } from "../client-resolver-mock";

describe("Commands", () => {
  describe("Filer", () => {
    describe("Move the cursor down of the filer", () => {
      const history = createLocationHistory({ records: [], maxRecordNumber: 100 });
      const { dispatcher, clientResolver } = createResolverMocks();

      it("throw error when pass undefined as argument", async () => {
        const command = C.createCommand();

        await expect(command.execute(dispatcher, undefined)).rejects.toThrowError();
      });

      it("do not call anything when filer is not initialized", async () => {
        const command = C.createCommand();
        const state = AppState.empty();
        await command.execute(dispatcher, { state, clientResolver });
        expect(dispatcher.dispatch).not.toBeCalled();
      });

      it("call dispatch function with argument when filer initialized", async () => {
        const command = C.createCommand();
        const state = AppState.empty();
        state.fileList.currentSide = Side.Left;
        state.fileList.left = createFiler({
          id: "id",
          name: "name",
          items: [],
          location: "test",
          currentCursorIndex: 0,
          history,
        });

        await command.execute(dispatcher, { state, clientResolver });
        expect(dispatcher.dispatch).toBeCalled();
      });

      it("moves filer cursor down", async () => {
        const command = C.createCommand();
        const state = AppState.empty();
        state.fileList.currentSide = Side.Left;
        state.fileList.left = createFiler({
          id: "id",
          name: "name",
          items: [],
          location: "test",
          currentCursorIndex: 0,
          history,
        });
        const spy = jest.spyOn(state.fileList.left, "moveIndex");

        await command.execute(dispatcher, { state, clientResolver });
        expect(spy).toBeCalledWith(Direction.Down);
      });
    });
  });
});
