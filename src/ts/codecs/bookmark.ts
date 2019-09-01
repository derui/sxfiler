import { Bookmark, createBookmark } from "@/domains/bookmark";

// define codec that is between filer domain and RPC
export type TypeOnRPC = {
  id: string;
  path: string;
  order: number;
};

/**
   encode node object from RPC to frontend domain.

   @param obj JSON representation for node
   @return Node object
 */
export const encode = function encode(obj: TypeOnRPC): Bookmark {
  return createBookmark(obj);
};
