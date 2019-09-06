import { AppAction, ActionTypes } from "./type";
import { Candidate } from "@/domains/candidate";
import { UIContext } from "@/types/ui-context";

type CloseAction = AppAction<ActionTypes.COMPLETER_CLOSE, { context: UIContext }>;
type CursorUpAction = AppAction<ActionTypes.COMPLETER_CURSOR_UP>;
type CursorDownAction = AppAction<ActionTypes.COMPLETER_CURSOR_DOWN>;
type OpenAction = AppAction<ActionTypes.COMPLETER_OPEN, { title: string; context: UIContext }>;
type ReplaceCandidateAction = AppAction<
  ActionTypes.COMPLETER_REPLACE_CANDIDATES,
  {
    candidates: Candidate[];
  }
>;

export const cursorUp = function cursorUp(): CursorUpAction {
  return { type: ActionTypes.COMPLETER_CURSOR_UP };
};

export const cursorDown = function cursorDown(): CursorDownAction {
  return { type: ActionTypes.COMPLETER_CURSOR_DOWN };
};

export const replaceCandidates = function replaceCandidates(candidates: Candidate[]): ReplaceCandidateAction {
  return { type: ActionTypes.COMPLETER_REPLACE_CANDIDATES, candidates };
};

export const open = function open(title: string, context: UIContext): OpenAction {
  return { type: ActionTypes.COMPLETER_OPEN, title, context };
};

export const close = function close(context: UIContext): CloseAction {
  return { type: ActionTypes.COMPLETER_CLOSE, context };
};

export type Actions = CursorUpAction | CursorDownAction | ReplaceCandidateAction | OpenAction | CloseAction;
