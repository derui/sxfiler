import * as C from "./finder-select";
import * as AppState from "@/states";
import { Side } from "@/states/file-list";
import * as filerActions from "@/actions/filer";
import * as completerActions from "@/actions/completer";
import { replaceCandidates } from "@/domains/completion";
import { createCandidate } from "@/domains/candidate";
import { compose } from "redux";
import * as CompleterState from "@/states/completer";
import { UIContext } from "@/types/ui-context";
import { createResolverMocks } from "../client-resolver-mock";

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
      const { dispatcher, clientResolver } = createResolverMocks();

      afterEach(jest.clearAllMocks);

      it("throw error when pass undefined as argument", async () => {
        const command = C.createCommand();

        await expect(command.execute(dispatcher, undefined)).rejects.toThrowError();
      });

      it("should dispatch action with the selected item", async () => {
        const command = C.createCommand();

        await command.execute(dispatcher, { state, clientResolver });
        expect(dispatcher.dispatch).toHaveBeenCalledWith(completerActions.close(UIContext.ForFinder));
        expect(dispatcher.dispatch).toHaveBeenCalledWith(filerActions.select(Side.Left, "id"));
      });
    });
  });
});
