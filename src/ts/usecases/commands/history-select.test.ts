import * as C from "./history-select";
import * as AppState from "@/states";
import { Side } from "@/states/file-list";
import { createFiler } from "@/domains/filer";
import { Apis } from "@/apis";
import { actions } from "@/actions/filer";
import * as historyActions from "@/actions/history";
import { createLocationHistory } from "@/domains/location-history";
import { createCompletion, moveCursor } from "@/domains/completion";
import { createCandidate } from "@/domains/candidate";

describe("Commands", () => {
  describe("Filer", () => {
    const state = AppState.empty();
    state.history = {
      side: Side.Left,
      opened: true,
      completion: createCompletion({ cursor: 0, candidates: [createCandidate({ id: "id", value: "value" })] }),
    };

    describe("Move to the parent of the filer", () => {
      const history = createLocationHistory({ records: [], maxRecordNumber: 100 });

      it("throw error when pass undefined as argument", async () => {
        const command = C.createCommand();
        const dispatcher = jest.fn();

        await expect(command.execute(dispatcher as any, undefined)).rejects.toThrowError();
      });

      it("call API to jump location of the filer", async () => {
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

        await command.execute(dispatcher as any, { state, client: client as any });
        expect(client.call).toBeCalledWith(Apis.Filer.Jump, { location: "value", name: Side.Left });
      });

      it("update a filer after moving directory upward", async () => {
        const command = C.createCommand();
        const dispatcher = {
          dispatch: jest.fn(),
        };
        const filer = createFiler({
          id: "id",
          name: "name",
          items: [],
          location: "test",
          currentCursorIndex: 0,
          history,
        });
        const client = {
          call: jest.fn().mockReturnValue(filer),
        };

        await command.execute(dispatcher, { state, client: client as any });
        expect(dispatcher.dispatch).toHaveBeenCalledWith(historyActions.close());
        expect(dispatcher.dispatch).toHaveBeenCalledWith(actions.load({ filer }));
      });

      it("should not call API if do not select any candidate", async () => {
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

        const newState = {
          ...state,
          history: { ...state.history, completion: moveCursor(1)(state.history.completion) },
        };
        client.call.mockResolvedValue(filer);

        await command.execute(dispatcher, { state: newState, client: client as any });
        expect(dispatcher.dispatch).not.toBeCalledTimes(1);
      });
    });
  });
});
