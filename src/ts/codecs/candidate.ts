import { Candidate, createCandidate } from "../domains/candidate";

// define codec that is between filer domain and RPC
export type TypeOnRPC = {
  start: number;
  length: number;
  value: {
    id: string;
    value: string;
  };
};

/**
   encode node object from RPC to frontend domain.

   @param obj JSON representation for node
   @return Node object
 */
export const encode = (obj: TypeOnRPC): Candidate => {
  return createCandidate({ id: obj.value.id, value: obj.value.value, start: obj.start, length: obj.length });
};
