import * as C from "./keymap-reload";
import * as AppState from "@/states";
import { Apis } from "@/apis";
import * as actions from "@/actions/key-map";
import { createKeymap } from "@/domains/keymap";
import { createResolverMocks } from "../client-resolver-mock";

const keyMap = createKeymap([{ when: { contexts: [] }, key: "k", action: "action" }]);

describe("Commands", () => {
  describe("Key map", () => {
    describe("reload key map on the server", () => {
      const { dispatcher, apiClient, clientResolver } = createResolverMocks();

      afterEach(jest.clearAllMocks);

      it("throw error when pass undefined as argument", async () => {
        const command = C.createCommand();

        await expect(command.execute(dispatcher, undefined)).rejects.toThrowError();
      });

      it("call API to reload key map on the server", async () => {
        const command = C.createCommand();
        const state = AppState.empty();
        apiClient.call.mockResolvedValue(keyMap);

        await command.execute(dispatcher, { state, clientResolver });
        expect(apiClient.call).toBeCalledWith(Apis.Keymap.Reload, {});
      });

      it("dispatch an action to reload keymap", async () => {
        const command = C.createCommand();
        const state = AppState.empty();

        apiClient.call.mockResolvedValue(keyMap);

        await command.execute(dispatcher, { state, clientResolver });
        expect(dispatcher.dispatch).toBeCalledWith(actions.updateKeymap(keyMap));
      });
    });
  });
});
