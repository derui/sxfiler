import * as C from "./history-close";
import * as AppState from "@/states";
import * as actions from "@/actions/completer";
import { UIContext } from "@/types/ui-context";
import { createResolverMocks } from "../client-resolver-mock";

describe("Commands", () => {
  describe("Completer", () => {
    describe("Close completion", () => {
      const { dispatcher, clientResolver } = createResolverMocks();

      afterEach(jest.clearAllMocks);

      it("throw error when pass undefined as argument", async () => {
        const command = C.createCommand();

        await expect(command.execute(dispatcher, undefined)).rejects.toThrowError();
      });

      it("should dispatch close action", async () => {
        const command = C.createCommand();

        const state = AppState.empty();

        await command.execute(dispatcher, { state, clientResolver });
        expect(dispatcher.dispatch).toBeCalledWith(actions.close(UIContext.ForHistory));
      });
    });
  });
});
