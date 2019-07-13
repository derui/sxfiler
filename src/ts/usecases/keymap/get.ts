// defines use case to initialize filer
import { Actions } from "@/actions";
import { actions } from "@/actions/key-map";
import { Dispatcher } from "@/types";
import { UseCaseLike } from "@/usecases/type";
import { Client } from "@/libs/json-rpc/client";
import { ApiMethod } from "@/apis";
import { Apis } from "@/apis";

export type UseCase = UseCaseLike<Actions>;

type UseCaseInner = UseCase & {
  client: Client<ApiMethod>;
};

export const createUseCase = (client: Client<ApiMethod>): UseCase => {
  return {
    client,
    async execute(dispatcher: Dispatcher<Actions>) {
      const keymap = await this.client.call(Apis.Keymap.Get, undefined);

      dispatcher.dispatch(actions.updateKeymap(keymap));
    },
  } as UseCaseInner;
};
