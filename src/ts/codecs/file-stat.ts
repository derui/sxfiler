import { FileStat as Domain, createFileStat } from "@/domains/file-stat";
import { createCapability, emptyCapability } from "@/domains/capability";
import { createMode } from "@/domains/mode";
import { FileStat, Mode, Capability } from "@/generated/filer_pb";

const defaultMode = createMode({
  owner: emptyCapability(),
  group: emptyCapability(),
  others: emptyCapability(),
});

const encodeCap = function encodeCapability(cap: Capability) {
  return createCapability({
    writable: cap.getWritable(),
    readable: cap.getReadable(),
    executable: cap.getExecutable(),
  });
};

const encodeMode = function encodeMode(mode: Mode) {
  const owner = mode.getOwner();
  const group = mode.getGroup();
  const others = mode.getOthers();

  return createMode({
    owner: owner ? encodeCap(owner) : emptyCapability(),
    group: group ? encodeCap(group) : emptyCapability(),
    others: others ? encodeCap(others) : emptyCapability(),
  });
};

/**
   encode node object from RPC to frontend domain.

   @param obj JSON representation for node
   @return Node object
 */
export const encode = function encode(obj: FileStat): Domain {
  const mode = obj.getMode();
  return createFileStat({
    mode: mode ? encodeMode(mode) : defaultMode,
    uid: obj.getUid(),
    gid: obj.getGid(),
    atime: obj.getAtime(),
    ctime: obj.getCtime(),
    mtime: obj.getMtime(),
    size: obj.getSize(),
    isDirectory: obj.getIsdirectory(),
    isFile: obj.getIsfile(),
    isSymlink: obj.getIssymlink(),
  });
};
