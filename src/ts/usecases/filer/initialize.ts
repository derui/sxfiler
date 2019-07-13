// defines use case to initialize filer
import { Actions } from "@/actions";
import { actions } from "@/actions/filer";
import { Dispatcher } from "@/types";
import { UseCaseLike } from "@/usecases/type";
import { Client } from "@/libs/json-rpc/client";
import { ApiMethod } from "@/apis";
import { Side } from "@/states/file-list";
import { Apis } from "@/apis";

export type UseCase = UseCaseLike<Actions, { location: string }>;

type UseCaseInner = UseCase & {
  client: Client<ApiMethod>;
};

export const createUseCase = (client: Client<ApiMethod>): UseCase => {
  return {
    client,
    async execute(dispatcher: Dispatcher<Actions>, { location }: { location: string }) {
      const initialLocation = location;

      let leftFiler = await this.client.call(Apis.Filer.Get, Side.Left);
      if (!leftFiler) {
        leftFiler = await this.client.call(Apis.Filer.Make, { initialLocation, name: Side.Left });
      }

      let rightFiler = await this.client.call(Apis.Filer.Get, Side.Right);
      if (!rightFiler) {
        rightFiler = await this.client.call(Apis.Filer.Make, { initialLocation, name: Side.Right });
      }

      return dispatcher.dispatch(actions.reload({ filers: [leftFiler, rightFiler] }));
    },
  } as UseCaseInner;
};
