import { Candidate } from "@/domains/candidate";

export type State = {
  // current key map
  readonly cursor: number;
  readonly candidates: Candidate[];
  readonly opened: boolean;
};

/** return empty state */
export const empty = (): State => {
  return { cursor: 0, candidates: [], opened: false };
};

/** open completion */
export const open = (state: State): State => {
  return { ...state, opened: true };
};

/** close completion */
export const close = (state: State): State => {
  return { ...state, opened: false };
};

/**
 * update candidates
 */
export const updateCandidates = (state: State, candidates: Candidate[]): State => {
  return { ...state, candidates };
};

/**
 * move cursor with amount
 */
export const moveCursor = (state: State, amount: number): State => {
  return { ...state, cursor: state.cursor + amount };
};
