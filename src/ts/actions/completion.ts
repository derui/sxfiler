import { AppAction } from "./type";
import { Candidate } from "../domains/candidate";

export enum ActionTypes {
  cursorUp = "completion_cursor_up",
  cursorDown = "completion_cursor_down",
  initializeCandidate = "completion_initialize_candidate",
}

type CursorUpAction = AppAction<ActionTypes.cursorUp>;
type CursorDownAction = AppAction<ActionTypes.cursorDown>;
type InitializeCandidateAction = AppAction<
  ActionTypes.initializeCandidate,
  {
    candidates: Candidate[];
  }
>;

const cursorUp = (): CursorUpAction => {
  return { type: ActionTypes.cursorUp };
};

const cursorDown = (): CursorDownAction => {
  return { type: ActionTypes.cursorDown };
};

const initializeCandidate = (candidates: Candidate[]): InitializeCandidateAction => {
  return { type: ActionTypes.initializeCandidate, candidates };
};

export const actions = { cursorUp, cursorDown, initializeCandidate };

export type Actions = CursorUpAction | CursorDownAction | InitializeCandidateAction;
