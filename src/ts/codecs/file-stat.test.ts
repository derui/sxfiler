import * as E from "./file-stat";
import { createFileStat } from "@/domains/file-stat";
import { createMode } from "@/domains/mode";
import { createCapability } from "@/domains/capability";
import { Capability, FileStat, Mode } from "@/generated/filer_pb";

function makeCap(writable: boolean, readable: boolean, executable: boolean) {
  const cap = new Capability({
    writable,
    readable,
    executable,
  });
  return cap;
}

const reqStat = () => {
  const mode = new Mode({
    owner: makeCap(true, true, true),
    group: makeCap(false, true, true),
    others: makeCap(false, true, false),
  });

  const ret = new FileStat({
    mode,
    uid: 1000,
    gid: 500,
    atime: "3",
    ctime: "4",
    mtime: "5",
    size: "10",
    isDirectory: false,
    isFile: true,
    isSymlink: false,
  });
  return ret;
};

describe("Object Codecs", () => {
  describe("File Stat", () => {
    it("can encode RPC domain to Frontend domain", () => {
      const obj = E.encode(reqStat());

      expect(obj).toEqual(
        createFileStat({
          mode: createMode({
            owner: createCapability({ readable: true, executable: true, writable: true }),
            group: createCapability({ readable: true, executable: true, writable: false }),
            others: createCapability({ readable: true, executable: false, writable: false }),
          }),
          uid: 1000,
          gid: 500,
          atime: "3",
          ctime: "4",
          mtime: "5",
          size: "10",
          isDirectory: false,
          isFile: true,
          isSymlink: false,
        })
      );
    });
  });
});
