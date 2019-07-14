import * as A from "@/actions/completion";
import * as Cs from "@/domains/candidate";
import * as S from "@/states/completion";
import * as C from "./completion";

describe("Reducers", () => {
  let state: S.State;

  beforeEach(() => {
    state = S.updateCandidates(S.empty(), [
      Cs.createCandidate({ id: "1", value: "value1", start: 0, length: 0 }),
      Cs.createCandidate({ id: "2", value: "value2", start: 0, length: 0 }),
    ]);
  });

  describe("Completion", () => {
    it("should increment cursor index when cursor down", () => {
      const v = C.reducer(state, A.cursorDown());
      expect(v.cursor).toEqual(1);
    });

    it("should decrement cursor index when cursor down", () => {
      const v = C.reducer(C.reducer(state, A.cursorDown()), A.cursorUp());
      expect(v.cursor).toEqual(0);
    });

    it("should reset state when new candidate given", () => {
      const candidates = [
        Cs.createCandidate({ id: "3", value: "value3", start: 0, length: 0 }),
        Cs.createCandidate({ id: "4", value: "value4", start: 0, length: 0 }),
      ];
      const v = C.reducer(C.reducer(state, A.cursorDown()), A.replaceCandidates(candidates));

      expect(v.candidates.map(v => v.plain())).toEqual(candidates.map(v => v.plain()));
    });
  });
});
