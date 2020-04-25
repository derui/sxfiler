import { createSelector } from "reselect";
import { State } from "./reducer";

export type SplittedCandidate = {
  id: string;
  before: string;
  matched: string;
  after: string;
};

const getCandidates = (state: State) => state.candidates;
const getCurrentCursor = (state: State) => state.currentCursorPosition;
const isOpened = (state: State) => state.opened;

/**
 * get candidates splitted by matching.
 */
export const getSplittedByMatching = createSelector(getCandidates, (candidates) => {
  return candidates.map((v) => {
    const value = v.value?.value || "";
    const start = v.start;
    const length = v.length;

    return {
      id: v.value?.id || "",
      before: value.slice(0, start),
      matched: value.slice(start, start + length),
      after: value.slice(start + length),
    };
  });
});

export const selectCurrentFocusedItem = createSelector(
  getCandidates,
  getCurrentCursor,
  isOpened,
  (candidates, cursor, opened) => {
    if (!opened) {
      return undefined;
    }
    return candidates[cursor.value];
  }
);
