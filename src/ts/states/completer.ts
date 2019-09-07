import { Completion, createCompletion } from "@/domains/completion";
import { Candidate } from "@/domains/candidate";

export type State = {
  readonly title: string;
  readonly opened: boolean;
  readonly completion: Completion;
};

export const empty = function empty(): State {
  return {
    title: "",
    opened: false,
    completion: createCompletion({}),
  };
};

export const open = function open(title: string) {
  return (state: State): State => ({ ...state, opened: true, title });
};

export const close = function close(state: State): State {
  return { ...state, completion: createCompletion({}), opened: false };
};

/**
   simple function to update completion in the state
 */
export const updateCompletion = function updateCompletion(completion: Completion) {
  return (state: State): State => ({ ...state, completion });
};

/**
   Get current selected candidate
 */
export const currentSelectedCandidate = function currentSelectedCandidate(state: State): Candidate | undefined {
  return state.completion.candidates[state.completion.cursor];
};
