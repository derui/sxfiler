export type CandidateObject = {
  readonly id: string;
  readonly value: string;
  readonly start: number;
  readonly length: number;
};

export type Candidate = CandidateObject & {
  /**
   * Get splitted candidate as 3 pieces by matched string that
   * has the one of before start of match, matched, and after matched.
   */
  splitByInput(): [string, string, string];

  /**
   * Get plain object.
   */
  plain(): CandidateObject;
};

type FactoryArg = Pick<CandidateObject, "id" | "value"> & Partial<Pick<CandidateObject, "start" | "length">>;

/**
 * Create candidate from candidate object.
 */
export const createCandidate = ({ id, value, start = 0, length = 0 }: FactoryArg): Candidate => {
  return {
    id,
    value,
    start: Math.max(0, Math.min(value.length - 1, start)),
    length: Math.max(0, Math.min(value.length, length)),
    splitByInput() {
      const beforeMatched = value.substring(0, start);
      const matched = value.substring(start, start + length);
      const afterMatched = value.substring(start + length);

      return [beforeMatched, matched, afterMatched];
    },

    plain() {
      return { id: this.id, value: this.value, start: this.start, length: this.length };
    },
  };
};
