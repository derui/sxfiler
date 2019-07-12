import { AppAction, ActionTypes } from "./type";

type Item = {
  id: string;
  value: string;
};

type CursorUpAction = AppAction<ActionTypes.COMPLETION_CURSOR_UP>;
type CursorDownAction = AppAction<ActionTypes.COMPLETION_CURSOR_DOWN>;
type InitializeCandidateAction = AppAction<
  ActionTypes.COMPLETION_INITIALIZE,
  {
    initialCandidates: Item[];
  }
>;
type ReadCandidatesAction = AppAction<
  ActionTypes.COMPLETION_READ,
  {
    input: string;
  }
>;

const cursorUp = (): CursorUpAction => {
  return { type: ActionTypes.COMPLETION_CURSOR_UP };
};

const cursorDown = (): CursorDownAction => {
  return { type: ActionTypes.COMPLETION_CURSOR_DOWN };
};

const initializeCandidate = (initialCandidates: Item[]): InitializeCandidateAction => {
  return { type: ActionTypes.COMPLETION_INITIALIZE, initialCandidates };
};

const readCandidates = (input: string): ReadCandidatesAction => {
  return { type: ActionTypes.COMPLETION_READ, input };
};

export const actions = { cursorUp, cursorDown, initializeCandidate, readCandidates };

export type Actions = CursorUpAction | CursorDownAction | InitializeCandidateAction | ReadCandidatesAction;
