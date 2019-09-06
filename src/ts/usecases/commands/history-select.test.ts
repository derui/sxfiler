import * as C from "./history-select";
import * as AppState from "@/states";
import { Side } from "@/states/file-list";
import { createFiler } from "@/domains/filer";
import { Apis } from "@/apis";
import * as actions from "@/actions/filer";
import * as completerActions from "@/actions/completer";
import { createLocationHistory } from "@/domains/location-history";
import { replaceCandidates } from "@/domains/completion";
import { createCandidate } from "@/domains/candidate";
import { compose } from "redux";
import * as CompleterState from "@/states/completer";
import { UIContext } from "@/types/ui-context";

describe("Commands", () => {
  describe("Filer", () => {
    const state = AppState.empty();
    state.completer = compose(
      CompleterState.open("title"),
      CompleterState.updateCompletion(
        replaceCandidates([createCandidate({ id: "id", value: "value" })])(state.completer.completion)
      )
    )(state.completer);

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
        expect(dispatcher.dispatch).toHaveBeenCalledWith(completerActions.close(UIContext.ForHistory));
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

        client.call.mockResolvedValue(filer);

        await command.execute(dispatcher, { state, client: client as any });
        expect(dispatcher.dispatch).not.toBeCalledTimes(1);
      });
    });
  });
});
