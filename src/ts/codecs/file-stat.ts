import { FileStat, createFileStat } from "@/domains/file-stat";
import { createCapability } from "@/domains/capability";
import { ModeObject, createMode } from "@/domains/mode";

// define codec that is between filer domain and RPC
export type TypeOnRPC = {
  mode: ModeObject;
  uid: number;
  gid: number;
  atime: string;
  ctime: string;
  mtime: string;
  size: string;
  isDirectory: boolean;
  isFile: boolean;
  isSymlink: boolean;
};

/**
   encode node object from RPC to frontend domain.

   @param obj JSON representation for node
   @return Node object
 */
export const encode = (obj: TypeOnRPC): FileStat => {
  return createFileStat({
    ...obj,
    mode: createMode({
      owner: createCapability(obj.mode.owner),
      group: createCapability(obj.mode.group),
      others: createCapability(obj.mode.others),
    }),
  });
};
