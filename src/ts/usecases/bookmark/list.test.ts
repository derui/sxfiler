import * as U from "./list";
import { Dispatcher } from "@/types";
import { Actions } from "@/actions";
import { Client } from "@/libs/json-rpc/client";
import { ApiMethod } from "@/apis";
import * as actions from "@/actions/bookmark";
import { createBookmark } from "@/domains/bookmark";

describe("UseCases", () => {
  describe("Bookmark", () => {
    describe("Get", () => {
      it("call api to list all bookmarks", async () => {
        const bookmarks = [createBookmark({ id: "id", path: "path", order: 1 })];

        const client: Client<ApiMethod> = {
          call: jest.fn().mockReturnValue(bookmarks),
          notify: jest.fn(),
        };
        const dispatcher: Dispatcher<Actions> = {
          dispatch: jest.fn(),
        };

        const useCase = U.createUseCase(client);
        await useCase.execute(dispatcher);

        expect(dispatcher.dispatch).toBeCalledWith(actions.updateBookmarks(bookmarks));
      });
    });
  });
});
