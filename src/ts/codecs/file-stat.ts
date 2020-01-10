import { FileStat as Domain, createFileStat } from "@/domains/file-stat";
import { createCapability, emptyCapability } from "@/domains/capability";
import { createMode } from "@/domains/mode";
import { FileStat, Mode, Capability } from "../generated/filer_pb";

const defaultMode = createMode({
  owner: emptyCapability(),
  group: emptyCapability(),
  others: emptyCapability(),
});

const encodeCap = function encodeCapability(cap: Capability) {
  return createCapability({
    writable: cap.writable,
    readable: cap.readable,
    executable: cap.executable,
  });
};

const encodeMode = function encodeMode(mode: Mode) {
  const owner = mode.owner;
  const group = mode.group;
  const others = mode.others;

  return createMode({
    owner: owner ? encodeCap(Capability.create(owner)) : emptyCapability(),
    group: group ? encodeCap(Capability.create(group)) : emptyCapability(),
    others: others ? encodeCap(Capability.create(others)) : emptyCapability(),
  });
};

/**
   encode node object from RPC to frontend domain.

   @param obj JSON representation for node
   @return Node object
 */
export const encode = function encode(obj: FileStat): Domain {
  const mode = obj.mode;
  return createFileStat({
    mode: mode ? encodeMode(Mode.create(mode)) : defaultMode,
    uid: obj.uid,
    gid: obj.gid,
    atime: obj.atime,
    ctime: obj.ctime,
    mtime: obj.mtime,
    size: obj.size,
    isDirectory: obj.isDirectory,
    isFile: obj.isFile,
    isSymlink: obj.isSymlink,
  });
};
