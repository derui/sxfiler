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

/**
   Get current selected candidate
 */
export function currentSelectedCandidate(state: State): Candidate | undefined {
  return state.completion.candidates[state.completion.cursor];
}
