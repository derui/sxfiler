import * as C from "./task-send-reply";
import * as AppState from "@/states";
import * as TaskInteractionState from "@/states/task-interaction";
import { Apis } from "@/apis";
import * as actions from "@/actions/task";
import { createSuggestions, SuggestionKind } from "@/domains/task-suggestion";
import { createResolverMocks } from "../client-resolver-mock";

describe("Commands", () => {
  describe("Task", () => {
    describe("send reply for the suggestion", () => {
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

      it("call API to reload key map on the server", async () => {
        const command = C.createCommand();

        await command.execute(dispatcher, { state, clientResolver });
        expect(apiClient.call).toBeCalledWith(Apis.Task.SendReply, state.taskInteraction.currentReply());
      });

      it("dispatch an action to send reply", async () => {
        const command = C.createCommand();

        await command.execute(dispatcher, { state, clientResolver });
        const reply = state.taskInteraction.currentReply();
        if (!reply) {
          return fail();
        }

        expect(dispatcher.dispatch).toBeCalledWith(actions.sendReply(reply));
      });
    });
  });
});
