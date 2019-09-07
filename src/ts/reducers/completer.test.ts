import { reducer } from "./completer";
import * as S from "@/states/completer";
import * as A from "@/actions/completer";
import { createCandidate } from "@/domains/candidate";
import { UIContext } from "@/types/ui-context";

describe("reducers", () => {
  describe("completer", () => {
    it("should open when it give open action", () => {
      const state = S.empty();
      const newState = reducer(state, A.open("title", UIContext.ForHistory));

      expect(newState.title).toEqual("title");
      expect(newState.opened).toBeTruthy;
    });

    it("should close when it give close action", () => {
      let state = S.empty();
      state = reducer(state, A.open("title", UIContext.ForHistory));
      state = reducer(state, A.close(UIContext.ForHistory));

      expect(state.opened).toBeFalsy;
    });

    const candidates = [
      createCandidate({ id: "id1", value: "value" }),
      createCandidate({ id: "id2", value: "value" }),
      createCandidate({ id: "id3", value: "value" }),
    ];

    it("should select candidate under current when give cursor-down action", () => {
      let state = S.empty();
      state = reducer(state, A.open("title", UIContext.ForHistory));
      state = reducer(state, A.replaceCandidates(candidates));
      state = reducer(state, A.cursorDown());

      expect(S.currentSelectedCandidate(state)).toEqual(candidates[1]);
    });

    it("should select candidate upto current when give cursor-up action", () => {
      let state = S.empty();
      state = reducer(state, A.open("title", UIContext.ForHistory));
      state = reducer(state, A.replaceCandidates(candidates));
      state = reducer(state, A.cursorDown());
      state = reducer(state, A.cursorUp());

      expect(S.currentSelectedCandidate(state)).toEqual(candidates[0]);
    });
  });
});
