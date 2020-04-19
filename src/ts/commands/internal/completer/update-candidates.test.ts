import * as C from "./update-candidates";
import * as M from "@/commands/client-resolver-mock";
import { actions } from "@/modules/completer";
import { also } from "@/libs/fn";
import { Candidate, Item } from "@/generated/completer_pb";
import { emptyState } from "@/modules";

describe("Commands", () => {
  describe("internal:completer:Update Candidates", () => {
    test("call action to update candidates", async () => {
      const mocks = M.createResolverMocks();
      const state = emptyState;

      const command = C.createCommand();
      const payload = [
        also(new Candidate(), (v) => {
          v.setStart(0);
          v.setLength(0);
          v.setValue(new Item());
        }),
        also(new Candidate(), (v) => {
          v.setStart(1);
          v.setLength(1);
          v.setValue(new Item());
        }),
      ];

      await command.execute(mocks.dispatcher, { clientResolver: mocks.clientResolver, state }, { candidates: payload });

      expect(mocks.dispatcher.dispatch).toBeCalledWith(actions.updateCandidates(payload));
    });
  });
});
