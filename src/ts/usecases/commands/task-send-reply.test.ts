import * as C from "./task-send-reply";
import * as AppState from "@/states";
import * as TaskInteractionState from "@/states/task-interaction";
import { Apis } from "@/apis";
import * as actions from "@/actions/task";
import { createSuggestions, SuggestionKind } from "@/domains/task-suggestion";

describe("Commands", () => {
  describe("Task", () => {
    describe("send reply for the suggestion", () => {
      it("throw error when pass undefined as argument", async () => {
        const command = C.createCommand();
        const dispatcher = jest.fn();

        await expect(command.execute(dispatcher as any, undefined)).rejects.toThrowError();
      });

      let state = AppState.empty();
      state.taskInteraction = TaskInteractionState.giveSuggestions(
        state.taskInteraction,
        createSuggestions({
          taskId: "foo",
          itemName: "item",
          suggestions: [SuggestionKind.Overwrite],
        })
      );

      it("call API to reload key map on the server", async () => {
        const command = C.createCommand();
        const dispatcher = {
          dispatch: jest.fn(),
        };
        const client = {
          call: jest.fn(),
        };

        await command.execute(dispatcher as any, { state, client: client as any });
        expect(client.call).toBeCalledWith(Apis.Task.SendReply, state.taskInteraction.currentReply());
      });

      it("dispatch an action to send reply", async () => {
        const command = C.createCommand();
        const dispatcher = {
          dispatch: jest.fn(),
        };
        const client = {
          call: jest.fn(),
        };

        await command.execute(dispatcher as any, { state, client: client as any });
        const reply = state.taskInteraction.currentReply();
        if (!reply) {
          return fail();
        }

        expect(dispatcher.dispatch).toBeCalledWith(actions.sendReply(reply));
      });
    });
  });
});
