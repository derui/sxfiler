import { Candidate, CandidateObject, createCandidate } from "./candidate";
import { between } from "@/utils";

export type CompletionObject = {
  // current key map
  readonly cursor: number;
  readonly candidates: CandidateObject[];
};

export type Completion = {
  // current key map
  readonly cursor: number;
  readonly candidates: Candidate[];

  /**
     replace candidates with new candidates
   */
  replaceCandidates(candidates: Candidate[]): Completion;

  /**
     move cursor to amount value
   */
  moveCursor(amount: number): Completion;

  /**
     return plain object
   */
  plain(): CompletionObject;
};

function replaceCandidates(this: Completion, candidates: CandidateObject[]): Completion {
  return { ...this, candidates: candidates.map(createCandidate) };
}

function moveCursor(this: Completion, amount: number): Completion {
  return { ...this, cursor: between(this.cursor + amount, this.candidates.length - 1) };
}

function plain(this: Completion): CompletionObject {
  return { cursor: this.cursor, candidates: this.candidates.map(v => v.plain()) };
}

/** return empty state */
export const createCompletion = ({ cursor, candidates }: Partial<CompletionObject>): Completion => {
  if (cursor && candidates) {
    cursor = between(cursor, candidates.length);
  } else if (candidates) {
    cursor = 0;
  } else {
    cursor = 0;
    candidates = [];
  }
  return { cursor, candidates: candidates.map(createCandidate), replaceCandidates, plain, moveCursor };
};
