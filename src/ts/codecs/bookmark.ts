import { Bookmark as Domain, createBookmark } from "@/domains/bookmark";
import { Bookmark } from "@/generated/bookmark_pb";

/**
   encode node object from RPC to frontend domain.

   @param obj JSON representation for node
   @return Node object
 */
export const encode = function encode(obj: Bookmark): Domain {
  return createBookmark({
    id: obj.id,
    path: obj.path,
    order: obj.order,
  });
};
