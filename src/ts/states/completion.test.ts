import * as D from "@/domains/candidate";
import * as S from "./completion";

describe("States", () => {
  describe("Completions", () => {
    it("should move cursor to up", () => {
      const state = S.updateCandidates(S.empty(), [
        D.createCandidate({ id: "1", value: "value1", start: 0, length: 0 }),
        D.createCandidate({ id: "2", value: "value2", start: 0, length: 0 }),
      ]);

      expect(S.moveCursor(state, 1).cursor).toEqual(1);
    });

    it("should move cursor to down", () => {
      const state = S.moveCursor(
        S.updateCandidates(S.empty(), [
          D.createCandidate({ id: "1", value: "value1", start: 0, length: 0 }),
          D.createCandidate({ id: "2", value: "value2", start: 0, length: 0 }),
        ]),
        1
      );

      expect(S.moveCursor(state, -1).cursor).toEqual(0);
    });

    it("should not move cursor to up if index is already 0", () => {
      const state = S.updateCandidates(S.empty(), [
        D.createCandidate({ id: "1", value: "value1", start: 0, length: 0 }),
        D.createCandidate({ id: "2", value: "value2", start: 0, length: 0 }),
      ]);

      expect(S.moveCursor(state, -1).cursor).toEqual(0);
    });

    it("should not move cursor down if cursor location is end ", () => {
      const state = S.updateCandidates(S.empty(), [
        D.createCandidate({ id: "1", value: "value1", start: 0, length: 0 }),
        D.createCandidate({ id: "2", value: "value2", start: 0, length: 0 }),
      ]);

      expect(S.moveCursor(state, 2).cursor).toEqual(1);
    });
  });
});
