import { AppAction, ActionTypes } from "./type";
import { Candidate } from "@/domains/candidate";
import { Side } from "@/states/file-list";

type CloseAction = AppAction<ActionTypes.HISTORY_CLOSE>;
type CursorUpAction = AppAction<ActionTypes.HISTORY_CURSOR_UP>;
type CursorDownAction = AppAction<ActionTypes.HISTORY_CURSOR_DOWN>;
type OpenAction = AppAction<ActionTypes.HISTORY_OPEN, { side: Side }>;
type ReplaceCandidateAction = AppAction<
  ActionTypes.HISTORY_REPLACE_CANDIDATES,
  {
    candidates: Candidate[];
  }
>;

export function cursorUp(): CursorUpAction {
  return { type: ActionTypes.HISTORY_CURSOR_UP };
}

export function cursorDown(): CursorDownAction {
  return { type: ActionTypes.HISTORY_CURSOR_DOWN };
}

export function replaceCandidates(candidates: Candidate[]): ReplaceCandidateAction {
  return { type: ActionTypes.HISTORY_REPLACE_CANDIDATES, candidates };
}

export function open(side: Side): OpenAction {
  return { type: ActionTypes.HISTORY_OPEN, side };
}

export function close(): CloseAction {
  return { type: ActionTypes.HISTORY_CLOSE };
}

export type Actions = CursorUpAction | CursorDownAction | ReplaceCandidateAction | OpenAction | CloseAction;
