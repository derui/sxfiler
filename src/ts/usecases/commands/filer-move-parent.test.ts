import * as C from "./filer-move-parent";
import * as AppState from "@/states";
import { Side } from "@/states/file-list";
import { createFiler } from "@/domains/filer";
import { Apis } from "@/apis";
import * as actions from "@/actions/filer";
import { createLocationHistory } from "@/domains/location-history";

describe("Commands", () => {
  describe("Filer", () => {
    describe("Move to the parent of the filer", () => {
      const history = createLocationHistory({ records: [], maxRecordNumber: 100 });

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
        client.call.mockResolvedValue(
          createFiler({ id: "id", name: "name", items: [], location: "test", currentCursorIndex: 0, history })
        );
        const state = AppState.empty();
        state.fileList.currentSide = Side.Left;

        await command.execute(dispatcher as any, { state, client: client as any });
        expect(client.call).toBeCalledWith(Apis.Filer.MoveParent, Side.Left);
      });

      it("update a filer after moving directory upward", async () => {
        const command = C.createCommand();
        const dispatcher = {
          dispatch: jest.fn(),
        };
        const client = {
          call: jest.fn(),
        };
        const filer = createFiler({
          id: "id",
          name: "name",
          items: [],
          location: "test",
          currentCursorIndex: 0,
          history,
        });
        const state = AppState.empty();
        state.fileList.currentSide = Side.Left;

        client.call.mockResolvedValue(filer);

        await command.execute(dispatcher as any, { state, client: client as any });
        await expect(dispatcher.dispatch).toBeCalledWith(actions.load({ filer }));
      });
    });
  });
});
