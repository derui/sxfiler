import * as C from "./filer-move-parent";
import * as AppState from "@/states";
import { Side } from "@/states/file-list";
import { createFiler } from "@/domains/filer";
import { Apis } from "@/apis";
import * as actions from "@/actions/filer";
import { createLocationHistory } from "@/domains/location-history";
import { createResolverMocks } from "../client-resolver-mock";

describe("Commands", () => {
  describe("Filer", () => {
    describe("Move to the parent of the filer", () => {
      const history = createLocationHistory({ records: [], maxRecordNumber: 100 });
      const { dispatcher, apiClient, clientResolver } = createResolverMocks();

      it("throw error when pass undefined as argument", async () => {
        const command = C.createCommand();

        await expect(command.execute(dispatcher, undefined)).rejects.toThrowError();
      });

      it("call API to move location of a filer", async () => {
        const command = C.createCommand();
        apiClient.call.mockResolvedValue(
          createFiler({ id: "id", name: "name", items: [], location: "test", currentCursorIndex: 0, history })
        );
        const state = AppState.empty();
        state.fileList.currentSide = Side.Left;

        await command.execute(dispatcher, { state, clientResolver });
        expect(apiClient.call).toBeCalledWith(Apis.Filer.MoveParent, Side.Left);
      });

      it("update a filer after moving directory upward", async () => {
        const command = C.createCommand();
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

        apiClient.call.mockResolvedValue(filer);

        await command.execute(dispatcher, { state, clientResolver });
        expect(dispatcher.dispatch).toBeCalledWith(actions.load({ filer }));
      });
    });
  });
});
