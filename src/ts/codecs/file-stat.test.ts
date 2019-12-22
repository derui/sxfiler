import * as E from "./file-stat";
import { createFileStat } from "@/domains/file-stat";
import { createMode } from "@/domains/mode";
import { createCapability } from "@/domains/capability";
import { Capability, FileStat, Mode } from "@/generated/filer_pb";

function makeCap(writable: boolean, readable: boolean, executable: boolean) {
  const cap = new Capability();
  cap.setWritable(writable);
  cap.setReadable(readable);
  cap.setExecutable(executable);
  return cap;
}

const reqStat = () => {
  const ret = new FileStat();
  const mode = new Mode();

  mode.setOwner(makeCap(true, true, true));
  mode.setGroup(makeCap(false, true, true));
  mode.setOthers(makeCap(false, true, false));
  ret.setMode(mode);
  ret.setUid(1000);
  ret.setGid(500);
  ret.setAtime("3");
  ret.setCtime("4");
  ret.setMtime("5");
  ret.setSize("10");
  ret.setIsdirectory(false);
  ret.setIsfile(true);
  ret.setIssymlink(false);
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
