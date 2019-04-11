// defines use case to initialize key map
import { Actions } from "../../actions";
import { actions } from "../../actions/keymap";
import { Dispatcher } from "../../types";
import { UseCaseLike } from "../type";
import { Client } from "../../libs/json-rpc/client";
import { ApiMethod } from "../../apis";
import { Apis } from "../../apis";

export type UseCase = UseCaseLike<Actions>;

export const createUseCase = (client: Client<ApiMethod>): UseCase => {
  return {
    client,
    async execute(dispatcher: Dispatcher<Actions>) {
      const keymap = await this.client.call(Apis.Keymap.AddContext, { context: "onFileTree" });

      dispatcher.dispatch(actions.getKeymap({ keymap }));
    },
  } as UseCase & {
    client: Client<ApiMethod>;
  };
};
