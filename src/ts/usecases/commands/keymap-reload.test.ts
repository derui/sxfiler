import * as C from "./keymap-reload";
import * as AppState from "../../states";
import { Apis } from "../../apis";
import { actions } from "../../actions/key-map";
import { createKeymap } from "../../domains/keymap";

const keyMap = createKeymap([{ key: "k", action: "action" }]);

describe("Commands", () => {
  describe("Key map", () => {
    describe("reload key map on the server", () => {
      it("throw error when pass undefined as argument", async () => {
        const command = C.createCommand();
        const dispatcher = jest.fn();

        await expect(command.execute(dispatcher as any, undefined)).rejects.toThrowError();
      });

      it("call API to reload key map on the server", async () => {
        const command = C.createCommand();
        const state = AppState.empty();
        const dispatcher = {
          dispatch: jest.fn(),
        };
        const client = {
          call: jest.fn(),
        };
        client.call.mockResolvedValue(keyMap);

        await command.execute(dispatcher as any, { state, client: client as any });
        expect(client.call).toBeCalledWith(Apis.Keymap.Reload, {});
      });

      it("dispatch an action to reload keymap", async () => {
        const command = C.createCommand();
        const dispatcher = {
          dispatch: jest.fn(),
        };
        const client = {
          call: jest.fn(),
        };
        const state = AppState.empty();

        client.call.mockResolvedValue(keyMap);

        await command.execute(dispatcher as any, { state, client: client as any });
        await expect(dispatcher.dispatch).toBeCalledWith(actions.updateKeymap(keyMap));
      });
    });
  });
});
