import { AppAction, ActionTypes } from "./type";
import { Candidate } from "@/domains/candidate";

type SelectAction = AppAction<ActionTypes.HISTORY_SELECT>;
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

export type Actions = CursorUpAction | CursorDownAction | ReplaceCandidateAction;
