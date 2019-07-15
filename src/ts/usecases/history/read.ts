import { Actions } from "@/actions";
import * as H from "@/actions/history";
import { Dispatcher } from "@/types";
import { UseCaseLike } from "@/usecases/type";
import { Client } from "@/libs/json-rpc/client";
import { ApiMethod, Apis } from "@/apis";

export type UseCase = UseCaseLike<Actions, { input: string }>;

type UseCaseInner = UseCase & {
  client: Client<ApiMethod>;
};

export const createUseCase = (client: Client<ApiMethod>): UseCase => {
  return {
    client,
    async execute(dispatcher: Dispatcher<Actions>, args: { input: string }) {
      const candidates = await this.client.call(Apis.Completion.Read, { input: args.input });

      dispatcher.dispatch(H.replaceCandidates(candidates));
    },
  } as UseCaseInner;
};
