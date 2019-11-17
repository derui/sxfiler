import { createCommand } from "./completer-cursor-up";
import * as AppState from "@/states";
import * as actions from "@/actions/completer";
import { createResolverMocks } from "../client-resolver-mock";

describe("UseCase", () => {
  describe("Finder", () => {
    describe("Cursor up", () => {
      const { dispatcher, clientResolver } = createResolverMocks();

      afterEach(jest.clearAllMocks);

      it("should throw error", async () => {
        const command = createCommand();

        await expect(command.execute(dispatcher, undefined)).rejects.toThrowError();
      });

      it("should dispatch cursor up action", async () => {
        const command = createCommand();

        const state = AppState.empty();

        await command.execute(dispatcher, { state, clientResolver });
        expect(dispatcher.dispatch).toBeCalledWith(actions.cursorUp());
      });
    });
  });
});
