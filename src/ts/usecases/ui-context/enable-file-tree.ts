// defines use case to initialize key map
import { Actions } from "../../actions";
import { actions } from "../../actions/ui-context";
import { Dispatcher } from "../../types";
import { UseCaseLike } from "../type";
import { Client } from "../../libs/json-rpc/client";
import { ApiMethod } from "../../apis";
import { Apis } from "../../apis";
import UIContext from "../../types/ui-context";

export type UseCase = UseCaseLike<Actions>;

export const createUseCase = (client: Client<ApiMethod>): UseCase => {
  return {
    client,
    async execute(dispatcher: Dispatcher<Actions>, args) {
      if (!args) {
        throw Error("Need context to initialize");
      }
      const keymap = await this.client.call(Apis.Keymap.AddContext, { context: UIContext.OnFileTree });

      dispatcher.dispatch(actions.enableFileTree({ keymap }));
    },
  } as UseCase & {
    client: Client<ApiMethod>;
  };
};
