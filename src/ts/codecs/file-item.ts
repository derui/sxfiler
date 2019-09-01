import { FileItem, createFileItem } from "@/domains/file-item";
import { encode as encodeFileStat, TypeOnRPC as FileStatOnRPC } from "./file-stat";

// define codec that is between filer domain and RPC

export type FileItemOnRPC = {
  id: string;
  parent: string;
  name: string;
  fullPath: string;
  stat: FileStatOnRPC;
  linkPath?: string;
};

/**
   encode node object from RPC to frontend domain.

   @param obj JSON representation for node
   @return Node object
 */
export const encode = function encode(obj: FileItemOnRPC & { marked: boolean }): FileItem {
  const { id, parent, fullPath, name, stat, linkPath, marked } = obj;

  return createFileItem({ id, name, fullPath, linkPath, stat: encodeFileStat(stat), parentDirectory: parent, marked });
};
