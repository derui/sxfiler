import { NodeObject, createNode } from "../domains/node";
import { encode as encodeFileStat, TypeOnRPC as FileStatOnRPC } from "./file-stat";

// define codec that is between filer domain and RPC

export type NodeOnRPC = {
  id: string;
  parent: string;
  name: string;
  stat: FileStatOnRPC;
  linkPath?: string;
};

/**
   encode node object from RPC to frontend domain.

   @param obj JSON representation for node
   @return Node object
 */
export const encode = (obj: NodeOnRPC & { marked: boolean }): NodeObject => {
  const stat = encodeFileStat(obj.stat);

  return createNode({ ...obj, stat, parentDirectory: obj.parent });
};
