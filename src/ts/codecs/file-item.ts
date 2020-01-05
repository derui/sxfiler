import { FileItem as Domain, createFileItem } from "@/domains/file-item";
import { encode as encodeFileStat } from "./file-stat";
import { FileItem, FileStat } from "@/generated/filer_pb";

/**
   encode node object from RPC to frontend domain.

   @param obj JSON representation for node
   @return Node object
 */
export const encode = function encode(obj: FileItem, marked: boolean): Domain {
  const stat = obj.stat;

  if (!stat) {
    throw Error("file item must have file stat");
  }

  return createFileItem({
    id: obj.id || "",
    name: obj.name || "",
    fullPath: obj.fullPath || "",
    linkPath: obj.hasLinkPath ? obj.linkPath || "" : undefined,
    stat: encodeFileStat(FileStat.create(stat)),
    parentDirectory: obj.parent || "",
    marked: marked,
  });
};
