import * as C from "./finder-select";
import * as AppState from "@/states";
import { Side } from "@/states/file-list";
import { createFiler } from "@/domains/filer";
import * as filerActions from "@/actions/filer";
import * as completerActions from "@/actions/completer";
import { createLocationHistory } from "@/domains/location-history";
import { replaceCandidates } from "@/domains/completion";
import { createCandidate } from "@/domains/candidate";
import { compose } from "redux";
import * as CompleterState from "@/states/completer";
import { UIContext } from "@/types/ui-context";

describe("Commands", () => {
  describe("Finder", () => {
    const state = AppState.empty();
    state.completer = compose(
      CompleterState.open("title"),
      CompleterState.updateCompletion(
        replaceCandidates([createCandidate({ id: "id", value: "value" })])(state.completer.completion)
      )
    )(state.completer);

    describe("Select an item in finder", () => {
      const history = createLocationHistory({ records: [], maxRecordNumber: 100 });

      it("throw error when pass undefined as argument", async () => {
        const command = C.createCommand();
        const dispatcher = jest.fn();

        await expect(command.execute(dispatcher as any, undefined)).rejects.toThrowError();
      });

      it("should dispatch action with the selected item", async () => {
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
        expect(dispatcher.dispatch).toHaveBeenCalledWith(completerActions.close(UIContext.ForFinder));
        expect(dispatcher.dispatch).toHaveBeenCalledWith(filerActions.select(Side.Left, "id"));
      });
    });
  });
});
