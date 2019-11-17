import * as C from "./task-select-next-reply";
import * as AppState from "@/states";
import * as actions from "@/actions/task";
import * as State from "@/states/task-interaction";
import { createSuggestions, SuggestionKind } from "@/domains/task-suggestion";
import { createResolverMocks } from "../client-resolver-mock";

describe("Commands", () => {
  describe("Task", () => {
    describe("Select the next reply from the current reply", () => {
      const { dispatcher, clientResolver } = createResolverMocks();

      afterEach(jest.clearAllMocks);

      it("throw error when pass undefined as argument", async () => {
        const command = C.createCommand();

        await expect(command.execute(dispatcher, undefined)).rejects.toThrowError();
      });

      it("call action to select interaction", async () => {
        const command = C.createCommand();
        const state = AppState.empty();
        state.taskInteraction = State.giveSuggestions(
          State.empty(),
          createSuggestions({
            taskId: "task",
            itemName: "node",
            suggestions: [SuggestionKind.Overwrite, SuggestionKind.Rename],
          })
        );
        state.taskInteraction = State.selectReply(state.taskInteraction, 0);

        await command.execute(dispatcher, { state, clientResolver });
        expect(dispatcher.dispatch).toBeCalledWith(actions.selectReply(1));
      });

      it("do not anything when no any reply", async () => {
        const command = C.createCommand();
        const state = AppState.empty();

        await command.execute(dispatcher, { state, clientResolver });
        expect(dispatcher.dispatch).not.toBeCalled;
      });
    });
  });
});
