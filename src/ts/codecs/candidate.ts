import { Candidate as Domain, createCandidate } from "@/domains/candidate";
import { Candidate } from "../generated/completion_pb";

/**
   encode node object from RPC to frontend domain.

   @param obj JSON representation for node
   @return Node object
 */
export const encode = function encode(obj: Candidate): Domain {
  return createCandidate({
    id: obj.value?.id || "",
    value: obj.value?.value || "",
    start: obj.start,
    length: obj.length,
  });
};
