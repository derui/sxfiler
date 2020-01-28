import * as S from "./selectors";
import { Candidate } from "@/generated/completer_pb";
import { State } from "./reducer";

function getCandidate(id: string, value: string, start: number = 0, length: number = 0): Candidate.AsObject {
  return {
    start,
    length,
    value: {
      id,
      value,
    },
  };
}

describe("Modules", () => {
  describe("Completer", () => {
    describe("Selectors", () => {
      test("get splitted items", () => {
        const state = {
          candidates: [getCandidate("id1", "value"), getCandidate("id2", "value2", 1, 3)],
        } as State;

        const ret = S.getSplittedByMatching(state);

        expect(ret).toEqual([
          { id: "id1", before: "", matched: "", after: "value" },
          { id: "id2", before: "v", matched: "alu", after: "e2" },
        ]);
      });
    });
  });
});
