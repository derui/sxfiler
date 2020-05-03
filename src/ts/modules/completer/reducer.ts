import { ActionTypes } from "./types";
import { Actions } from "./actions";
import * as N from "@/types/natural-number";
import { Candidate } from "@/generated/completer_pb";

// state of type. Please redefine to what you want.
export type State = Readonly<{
  currentCursorPosition: N.Type;
  candidates: Candidate.AsObject[];
  opened: boolean;
  title?: string;
}>;

export const emptyState: State = Object.freeze({
  currentCursorPosition: N.create(0),
  candidates: [],
  opened: false,
});

/**
 * reducer for `ActionTypes.CURSOR_DOWN`
 */
const cursorDown = (state: State): State => {
  const candidateSize = N.create(state.candidates.length - 1);
  return Object.freeze({ ...state, currentCursorPosition: N.min(candidateSize, N.inc(state.currentCursorPosition)) });
};

/**
 * reducer for `ActionTypes.CURSOR_UP`
 */
const cursorUp = (state: State): State => {
  return Object.freeze({ ...state, currentCursorPosition: N.dec(state.currentCursorPosition) });
};

/**
 * reducer for `ActionTypes.UPDATE_CANDIDATE`
 */
const updateCandidates = (state: State, candidates: Candidate[]): State => {
  return Object.freeze({
    ...state,
    candidates: candidates.map((v) => v.toObject()),
    currentCursorPosition: N.create(0),
  });
};

/**
 * reducer for `ActionTypes.OPEN`
 */
const open = (state: State, title: string): State => {
  return Object.freeze({ ...state, opened: true, title });
};

/**
 * reducer for `ActionTypes.CLOSE`
 */
const close = (state: State): State => {
  return Object.freeze({ ...state, opened: false });
};

export const reducer = (state: State = emptyState, action: Actions): State => {
  switch (action.type) {
    case ActionTypes.UPDATE_CANDIDATES:
      return updateCandidates(state, action.payload.candidates);
    case ActionTypes.CURSOR_DOWN:
      return cursorDown(state);
    case ActionTypes.CURSOR_UP:
      return cursorUp(state);
    case ActionTypes.OPEN:
      return open(state, action.payload.title);
    case ActionTypes.CLOSE:
      return close(state);
    default:
      return state;
  }
};
