import { Actions } from "@/actions";
import * as H from "@/actions/history";
import { Dispatcher } from "@/types";
import { UseCaseLike } from "@/usecases/type";
import { Client } from "@/libs/json-rpc/client";
import { ApiMethod, Apis } from "@/apis";

export type UseCase = UseCaseLike<Actions, { input: string }>;

export const createUseCase = (client: Client<ApiMethod>): UseCase => {
  return {
    async execute(dispatcher: Dispatcher<Actions>, args: { input: string }) {
      const candidates = await client.call(Apis.Completion.Read, { input: args.input });

      dispatcher.dispatch(H.replaceCandidates(candidates));
    },
  };
};
