import { Completion, createCompletion } from "@/domains/completion";
import { Side } from "./file-list";
import { Candidate } from "@/domains/candidate";

export type State = {
  readonly opened: boolean;
  readonly completion: Completion;
  readonly side: Side;
};

export const empty = (): State => {
  return {
    opened: false,
    completion: createCompletion({}),
    side: Side.Left,
  };
};

export const open = function open(side: Side) {
  return (state: State) => ({ ...state, opened: true, side });
};

export const close = function close(state: State): State {
  return { ...state, opened: false };
};

/**
   Get current selected candidate
 */
export const currentSelectedCandidate = function currentSelectedCandidate(state: State): Candidate | undefined {
  return state.completion.candidates[state.completion.cursor];
};
