import { FileItem, createFileItem } from "../domains/file-item";
import { encode as encodeFileStat, TypeOnRPC as FileStatOnRPC } from "./file-stat";

// define codec that is between filer domain and RPC

export type FileItemOnRPC = {
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
export const encode = (obj: FileItemOnRPC & { marked: boolean }): FileItem => {
  const stat = encodeFileStat(obj.stat);

  return createFileItem({ ...obj, stat, parentDirectory: obj.parent });
};