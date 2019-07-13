import * as C from "./task-select-previous-reply";
import * as AppState from "@/states";
import { actions } from "@/actions/task";
import * as State from "@/states/task-interaction";
import { createSuggestions, createSuggestion, SuggestionKind } from "@/domains/task-suggestion";

describe("Commands", () => {
  describe("Task", () => {
    describe("Select the previous reply from the current reply", () => {
      it("throw error when pass undefined as argument", async () => {
        const command = C.createCommand();
        const dispatcher = jest.fn();

        await expect(command.execute(dispatcher as any, undefined)).rejects.toThrowError();
      });

      it("call action to select interaction", async () => {
        const command = C.createCommand();
        const dispatcher = {
          dispatch: jest.fn(),
        };
        const state = AppState.empty();
        state.taskInteraction = State.giveSuggestions(
          State.empty(),
          createSuggestions({
            taskId: "task",
            nodeName: "node",
            suggestions: [
              createSuggestion({ kind: SuggestionKind.Overwrite }),
              createSuggestion({ kind: SuggestionKind.Rename }),
            ],
          })
        );
        state.taskInteraction = State.selectReply(state.taskInteraction, 1);

        await command.execute(dispatcher as any, { state, client: jest.fn() as any });
        await expect(dispatcher.dispatch).toBeCalledWith(actions.selectReply(0));
      });

      it("do not anything when no any reply", async () => {
        const command = C.createCommand();
        const dispatcher = {
          dispatch: jest.fn(),
        };
        const state = AppState.empty();

        await command.execute(dispatcher as any, { state, client: jest.fn() as any });
        await expect(dispatcher.dispatch).not.toBeCalled;
      });
    });
  });
});
