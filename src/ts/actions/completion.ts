import { AppAction, ActionTypes } from "./type";
import { Candidate } from "@/domains/candidate";

type CursorUpAction = AppAction<ActionTypes.COMPLETION_CURSOR_UP>;
type CursorDownAction = AppAction<ActionTypes.COMPLETION_CURSOR_DOWN>;
type ReplaceCandidateAction = AppAction<
  ActionTypes.COMPLETION_REPLACE_CANDIDATES,
  {
    candidates: Candidate[];
  }
>;

export const cursorUp = (): CursorUpAction => {
  return { type: ActionTypes.COMPLETION_CURSOR_UP };
};

export const cursorDown = (): CursorDownAction => {
  return { type: ActionTypes.COMPLETION_CURSOR_DOWN };
};

export const replaceCandidates = (candidates: Candidate[]): ReplaceCandidateAction => {
  return { type: ActionTypes.COMPLETION_REPLACE_CANDIDATES, candidates };
};

export type Actions = CursorUpAction | CursorDownAction | ReplaceCandidateAction;
