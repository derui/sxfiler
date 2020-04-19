import * as S from "./selectors";
import { Candidate } from "@/generated/completer_pb";
import { State } from "./reducer";
import * as N from "@/types/natural-number";

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

      test("get current focused item if opened", () => {
        const state = {
          candidates: [getCandidate("id1", "value"), getCandidate("id2", "value2", 1, 3)],
          opened: true,
          currentCursorPosition: N.create(1),
        } as State;

        const ret = S.selectCurrentFocusedItem(state);

        expect(ret).toEqual(getCandidate("id2", "value2", 1, 3));
      });

      test("return undefined if it closed", () => {
        const state = {
          candidates: [getCandidate("id1", "value"), getCandidate("id2", "value2", 1, 3)],
          opened: false,
          currentCursorPosition: N.create(1),
        } as State;

        const ret = S.selectCurrentFocusedItem(state);

        expect(ret).toBeUndefined();
      });
    });
  });
});
