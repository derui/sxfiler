import { Actions } from "@/actions";
import * as actions from "@/actions/bookmark";
import { Dispatcher } from "@/types";
import { UseCaseLike } from "@/usecases/type";
import { Client } from "@/libs/json-rpc/client";
import { ApiMethod, Apis } from "@/apis";

export type UseCase = UseCaseLike<Actions>;

type UseCaseInner = UseCase & {
  client: Client<ApiMethod>;
};

export const createUseCase = function createUseCase(client: Client<ApiMethod>): UseCase {
  return {
    client,
    async execute(dispatcher: Dispatcher<Actions>) {
      const bookmarks = await this.client.call(Apis.Bookmark.ListAll, undefined);

      dispatcher.dispatch(actions.updateBookmarks(bookmarks));
    },
  } as UseCaseInner;
};
