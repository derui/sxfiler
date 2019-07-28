import { createCommand } from "./history-cursor-up";
import * as AppState from "@/states";
import * as actions from "@/actions/history";

describe("UseCase", () => {
  describe("History cursor up", () => {
    it("should throw error", async () => {
      const command = createCommand();
      const dispatcher = jest.fn();

      await expect(command.execute(dispatcher as any, undefined)).rejects.toThrowError();
    });

    it("should dispatch cursor up action", async () => {
      const command = createCommand();
      const dispatcher = {
        dispatch: jest.fn(),
      };
      const client = {
        call: jest.fn(),
        notify: jest.fn(),
      };

      const state = AppState.empty();

      await command.execute(dispatcher, { state, client });
      expect(dispatcher.dispatch).toBeCalledWith(actions.cursorUp());
    });
  });
});
