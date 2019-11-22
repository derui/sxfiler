import * as C from "./quit";
import * as AppState from "@/states";
import { createResolverMocks } from "../client-resolver-mock";

describe("Commands", () => {
  describe("Global", () => {
    describe("Quit application", () => {
      const { dispatcher, appClient, clientResolver } = createResolverMocks();

      afterEach(jest.clearAllMocks);

      it("throw error when pass undefined as argument", async () => {
        const command = C.createCommand();

        await expect(command.execute(dispatcher, undefined)).rejects.toThrowError();
      });

      it("should call quit via appClient", async () => {
        const command = C.createCommand();
        const state = AppState.empty();

        await command.execute(dispatcher, { state, clientResolver });
        expect(appClient.quit).toBeCalledTimes(1);
      });
    });
  });
});
