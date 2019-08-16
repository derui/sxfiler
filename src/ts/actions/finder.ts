import { AppAction, ActionTypes } from "./type";
import { Candidate } from "@/domains/candidate";
import { Side } from "@/states/file-list";

type CloseAction = AppAction<ActionTypes.FINDER_CLOSE>;
type CloseWithSelectAction = AppAction<ActionTypes.FINDER_CLOSE_WITH_SELECT, { side: Side; itemId: string }>;
type CursorUpAction = AppAction<ActionTypes.FINDER_CURSOR_UP>;
type CursorDownAction = AppAction<ActionTypes.FINDER_CURSOR_DOWN>;
type OpenAction = AppAction<ActionTypes.FINDER_OPEN, { side: Side }>;
type ReplaceCandidateAction = AppAction<
  ActionTypes.FINDER_REPLACE_CANDIDATES,
  {
    candidates: Candidate[];
  }
>;

export const cursorUp = function cursorUp(): CursorUpAction {
  return { type: ActionTypes.FINDER_CURSOR_UP };
};

export const cursorDown = function cursorDown(): CursorDownAction {
  return { type: ActionTypes.FINDER_CURSOR_DOWN };
};

export const replaceCandidates = function replaceCandidates(candidates: Candidate[]): ReplaceCandidateAction {
  return { type: ActionTypes.FINDER_REPLACE_CANDIDATES, candidates };
};

export const open = function open(side: Side): OpenAction {
  return { type: ActionTypes.FINDER_OPEN, side };
};

export const close = function close(): CloseAction {
  return { type: ActionTypes.FINDER_CLOSE };
};
export const closeWithSelect = function closeWithSelect(side: Side, itemId: string): CloseWithSelectAction {
  return { type: ActionTypes.FINDER_CLOSE_WITH_SELECT, side, itemId };
};

export type Actions =
  | CursorUpAction
  | CursorDownAction
  | ReplaceCandidateAction
  | OpenAction
  | CloseAction
  | CloseWithSelectAction;
