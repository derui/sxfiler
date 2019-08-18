import * as U from "./finder-read";
import { Dispatcher } from "@/types";
import { Actions } from "@/actions";
import { Client } from "@/libs/json-rpc/client";
import { ApiMethod } from "@/apis";
import * as actions from "@/actions/finder";
import { createCandidate } from "@/domains/candidate";

describe("UseCases", () => {
  describe("Filer", () => {
    describe("Finder Read", () => {
      it("call api to read candidates", async () => {
        const candidates = [createCandidate({ id: "id", value: "foo", start: 0, length: 0 })];

        const client: Client<ApiMethod> = {
          call: jest.fn().mockReturnValue(candidates),
          notify: jest.fn(),
        };
        const dispatcher: Dispatcher<Actions> = {
          dispatch: jest.fn(),
        };

        const useCase = U.createUseCase(client);
        await useCase.execute(dispatcher, { input: "foo" });

        expect(dispatcher.dispatch).toBeCalledWith(actions.replaceCandidates(candidates));
      });
    });
  });
});
