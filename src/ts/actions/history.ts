import { AppAction, ActionTypes } from "./type";
import { Candidate } from "@/domains/candidate";

type OpenAction = AppAction<ActionTypes.HISTORY_OPEN>;
type CloseAction = AppAction<ActionTypes.HISTORY_CLOSE>;
type CursorUpAction = AppAction<ActionTypes.HISTORY_CURSOR_UP>;
type CursorDownAction = AppAction<ActionTypes.HISTORY_CURSOR_DOWN>;
type ReplaceCandidateAction = AppAction<
  ActionTypes.HISTORY_REPLACE_CANDIDATES,
  {
    candidates: Candidate[];
  }
>;

export const cursorUp = (): CursorUpAction => {
  return { type: ActionTypes.HISTORY_CURSOR_UP };
};

export const cursorDown = (): CursorDownAction => {
  return { type: ActionTypes.HISTORY_CURSOR_DOWN };
};

export const replaceCandidates = (candidates: Candidate[]): ReplaceCandidateAction => {
  return { type: ActionTypes.HISTORY_REPLACE_CANDIDATES, candidates };
};

export const open = (): OpenAction => {
  return { type: ActionTypes.HISTORY_OPEN };
};

export const close = (): CloseAction => {
  return { type: ActionTypes.HISTORY_CLOSE };
};

export type Actions = CursorUpAction | CursorDownAction | ReplaceCandidateAction | OpenAction | CloseAction;
