import { Filer, createFiler } from "../domains/filer";
import { NodeOnRPC, encode as encodeNode } from "./node";

// define codec that is between filer domain and RPC

export type FilerOnRPC = {
  id: string;
  name: string;
  fileTree: {
    location: string;
    nodes: NodeOnRPC[];
  };
  markedNodes: string[];
  sortOrder: string;
};

/**
   encode filer object from RPC to frontend domain.

   @param filer JSON representation for fller
   @return Filer object
 */
export const encode = (obj: FilerOnRPC): Filer => {
  const {
    id,
    name,
    fileTree: { location, nodes },
    markedNodes,
  } = obj;

  return createFiler({
    id,
    name,
    location,
    nodes: nodes
      .map(v => ({
        ...v,
        marked: markedNodes.includes(v.id),
      }))
      .map(encodeNode),
    currentCursorIndex: 0,
  });
};
