import { Candidate } from "./candidate";
import { between } from "@/utils";

export type Completion = {
  // current key map
  readonly cursor: number;
  readonly candidates: Candidate[];
};

/**
   replace candidates
 */
export const replaceCandidates = function replaceCandidates(candidates: Candidate[]) {
  return (state: Completion): Completion => {
    return { ...state, candidates: Array.from(candidates) };
  };
};

/**
   move cursor
 */
export const moveCursor = function moveCursor(amount: number) {
  return (state: Completion): Completion => {
    return { ...state, cursor: between(state.cursor + amount, state.candidates.length - 1) };
  };
};

/** return empty state */
export const createCompletion = function createCompletion({ cursor, candidates }: Partial<Completion>): Completion {
  if (cursor && candidates) {
    cursor = between(cursor, candidates.length);
  } else if (candidates) {
    cursor = 0;
  } else {
    cursor = 0;
    candidates = [];
  }
  return { cursor, candidates: Array.from(candidates) };
};
