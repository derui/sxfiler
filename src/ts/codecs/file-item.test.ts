import * as E from "./file-item";
import * as Es from "./file-stat";
import { createFileItem } from "@/domains/file-item";
import { FileItem, FileStat, Mode, Capability } from "@/generated/filer_pb";

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
  describe("File Item", () => {
    it("can encode RPC domain to Frontend domain", () => {
      const req = new FileItem({
        id: "node",
        name: "node",
        fullPath: "parent/node",
        parent: "parent",
        stat: reqStat(),
      });
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
