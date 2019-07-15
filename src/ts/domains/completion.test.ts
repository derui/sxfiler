import * as D from "@/domains/candidate";
import * as S from "./completion";

describe("Domains", () => {
  describe("Completion", () => {
    const candidates = [
      D.createCandidate({ id: "1", value: "value1", start: 0, length: 0 }),
      D.createCandidate({ id: "2", value: "value2", start: 0, length: 0 }),
    ];

    it("should create new object", () => {
      const state = S.createCompletion({});

      expect(state.plain()).toEqual({ cursor: 0, candidates: [] });
    });

    it("should reset cursor when candidates is not given", () => {
      const state = S.createCompletion({ cursor: 10 });

      expect(state.plain()).toEqual({ cursor: 0, candidates: [] });
    });

    it("should be 0 of cursor when cursor is not given", () => {
      const state = S.createCompletion({
        candidates,
      });

      expect(state.plain()).toEqual({ cursor: 0, candidates: candidates.map(v => v.plain()) });
    });

    it("should keep cursor when cursor and candidates are given", () => {
      const state = S.createCompletion({
        cursor: 1,
        candidates,
      });

      expect(state.plain()).toEqual({ cursor: 1, candidates: candidates.map(v => v.plain()) });
    });

    it("should move cursor to down", () => {
      const state = S.createCompletion({
        candidates: [
          D.createCandidate({ id: "1", value: "value1", start: 0, length: 0 }),
          D.createCandidate({ id: "2", value: "value2", start: 0, length: 0 }),
        ],
      });

      expect(state.moveCursor(1).cursor).toEqual(1);
    });

    it("should move cursor to up", () => {
      const state = S.createCompletion({
        cursor: 1,
        candidates: [
          D.createCandidate({ id: "1", value: "value1", start: 0, length: 0 }),
          D.createCandidate({ id: "2", value: "value2", start: 0, length: 0 }),
        ],
      });

      expect(state.moveCursor(-1).cursor).toEqual(0);
    });

    it("should replace candidate", () => {
      const state = S.createCompletion({});
      const replaced = state.replaceCandidates(candidates);

      expect(replaced.plain()).toEqual({ cursor: 0, candidates: candidates.map(v => v.plain()) });
    });
  });
});
