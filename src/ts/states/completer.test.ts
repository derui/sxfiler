import * as C from "./completer";
import { replaceCandidates, createCompletion } from "@/domains/completion";
import { createCandidate } from "@/domains/candidate";

describe("States", () => {
  describe("Completer", () => {
    it("get empty state", () => {
      const state = C.empty();

      expect(state.completion).toEqual(createCompletion({}));
      expect(state.opened).toBeFalsy;
    });

    it("should return undefined when the list of candidate is empty", () => {
      const state = C.empty();

      expect(C.currentSelectedCandidate(state)).toBeUndefined;
    });

    it("get the candidate on the cursor currently", () => {
      const state = C.empty();
      const candidate = createCandidate({ id: "foo", value: "value" });
      const completion = replaceCandidates([candidate])(state.completion);
      let newState = C.updateCompletion(completion)(state);

      expect(C.currentSelectedCandidate(newState)).toEqual(candidate);
    });
  });
});
