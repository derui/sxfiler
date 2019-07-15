import * as Action from "@/actions/history";
import { reducer } from "./history";
import { empty } from "@/states/history";
import { Side } from "@/states/file-list";
import { createCandidate } from "@/domains/candidate";

describe("Reducers", () => {
  describe("History", () => {
    it("should open history when execute open action", () => {
      const state = reducer(empty(), Action.open(Side.Right));

      expect(state.opened).toBeTruthy();
      expect(state.side).toEqual(Side.Right);
    });

    it("should close history when execute close action", () => {
      const state = reducer(reducer(empty(), Action.open(Side.Left)), Action.close());

      expect(state.opened).toBeFalsy();
    });

    it("should select next history when execute cursor down action", () => {
      const state = reducer(
        reducer(
          empty(),
          Action.replaceCandidates([
            createCandidate({ id: "1", value: "v1", start: 0, length: 0 }),
            createCandidate({ id: "2", value: "v2", start: 0, length: 0 }),
          ])
        ),
        Action.cursorDown()
      );

      expect(state.completion.cursor).toEqual(1);
    });

    it("should select previous history when execute cursor up action", () => {
      let state = reducer(
        empty(),
        Action.replaceCandidates([
          createCandidate({ id: "1", value: "v1", start: 0, length: 0 }),
          createCandidate({ id: "2", value: "v2", start: 0, length: 0 }),
        ])
      );
      state = reducer(state, Action.cursorDown());
      state = reducer(state, Action.cursorUp());

      expect(state.completion.cursor).toEqual(0);
    });
  });
});
