import { FileItem as Domain, createFileItem } from "@/domains/file-item";
import { encode as encodeFileStat } from "./file-stat";
import { FileItem } from "@/generated/filer_pb";

/**
   encode node object from RPC to frontend domain.

   @param obj JSON representation for node
   @return Node object
 */
export const encode = function encode(obj: FileItem, marked: boolean): Domain {
  const stat = obj.getStat();

  if (!stat) {
    throw Error("file item must have file stat");
  }

  return createFileItem({
    id: obj.getId(),
    name: obj.getName(),
    fullPath: obj.getFullpath(),
    linkPath: obj.getHaslinkpath() ? obj.getLinkpath() : undefined,
    stat: encodeFileStat(stat),
    parentDirectory: obj.getParent(),
    marked: marked,
  });
};
