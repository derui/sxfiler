import * as E from "./file-item";
import * as Es from "./file-stat";
import { createFileItem } from "@/domains/file-item";
import { FileItem, FileStat, Mode, Capability } from "@/generated/filer_pb";

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
  describe("File Item", () => {
    it("can encode RPC domain to Frontend domain", () => {
      const req = new FileItem();
      req.setId("node");
      req.setName("node");
      req.setFullpath("parent/node");
      req.setParent("parent");
      req.setStat(reqStat());
      const obj = E.encode(req, false);

      expect(obj).toEqual(
        createFileItem({
          id: "node",
          name: "node",
          fullPath: "parent/node",
          parentDirectory: "parent",
          stat: Es.encode(reqStat()),
          marked: false,
        })
      );
    });
  });
});
