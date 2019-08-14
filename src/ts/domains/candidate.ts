export type Candidate = {
  readonly id: string;
  readonly value: string;
  readonly start: number;
  readonly length: number;
};

type FactoryArg = Pick<Candidate, "id" | "value"> & Partial<Pick<Candidate, "start" | "length">>;

/**
 * Create candidate from candidate object.
 */
export const createCandidate = function createCandidate({ id, value, start = 0, length = 0 }: FactoryArg): Candidate {
  return {
    id,
    value,
    start: Math.max(0, Math.min(value.length - 1, start)),
    length: Math.max(0, Math.min(value.length, length)),
  };
};

/**
   split candidate that based on matching
 */
export const splitByMatching = function splitByMatching(state: Candidate) {
  const { value, start, length } = state;
  const beforeMatched = value.substring(0, start);
  const matched = value.substring(start, start + length);
  const afterMatched = value.substring(start + length);

  return [beforeMatched, matched, afterMatched];
};
