import * as U from "./get";
import { Dispatcher } from "../../types";
import { Actions } from "../../actions";
import { Client } from "../../libs/json-rpc/client";
import { ApiMethod } from "../../apis";
import * as actions from "../../actions/key-map";
import { createKeymap } from "../../domains/keymap";

describe("UseCases", () => {
  describe("Key map", () => {
    describe("Get", () => {
      it("call api to get whole key maps", async () => {
        const keymap = createKeymap([]);

        const client: Client<ApiMethod> = {
          call: jest.fn().mockReturnValue(keymap),
          notify: jest.fn(),
        };
        const dispatcher: Dispatcher<Actions> = {
          dispatch: jest.fn(),
        };

        const useCase = U.createUseCase(client);
        await useCase.execute(dispatcher);

        expect(dispatcher.dispatch).toBeCalledWith(actions.actions.updateKeymap(keymap));
      });
    });
  });
});
