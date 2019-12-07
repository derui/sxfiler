import * as C from "./task-cancel";
import * as AppState from "@/states";
import * as TaskInteractionState from "@/states/task-interaction";
import { Apis } from "@/apis";
import * as actions from "@/actions/task";
import { createSuggestions, SuggestionKind } from "@/domains/task-suggestion";
import { createResolverMocks } from "../client-resolver-mock";

describe("Commands", () => {
  describe("Task", () => {
    describe("cancel the task", () => {
      let state = AppState.empty();
      state.taskInteraction = TaskInteractionState.giveSuggestions(
        state.taskInteraction,
        createSuggestions({
          taskId: "foo",
          itemName: "item",
          suggestions: [SuggestionKind.Overwrite],
        })
      );

      const { dispatcher, clientResolver, apiClient } = createResolverMocks();

      afterEach(jest.clearAllMocks);

      it("throw error when pass undefined as argument", async () => {
        const command = C.createCommand();

        await expect(command.execute(dispatcher, undefined)).rejects.toThrowError();
      });

      it("call API to cancel the task", async () => {
        const command = C.createCommand();

        await command.execute(dispatcher, { state, clientResolver });
        expect(apiClient.call).toBeCalledWith(Apis.Task.Cancel, state.taskInteraction.currentTaskId);
      });

      it("dispatch an action to cancel the task", async () => {
        const command = C.createCommand();

        await command.execute(dispatcher, { state, clientResolver });
        const taskId = state.taskInteraction.currentTaskId;
        if (!taskId) {
          return fail();
        }

        expect(dispatcher.dispatch).toBeCalledWith(actions.canceled(taskId));
      });
    });
  });
});
