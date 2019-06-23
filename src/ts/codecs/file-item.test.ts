import * as E from "./file-item";

const stat = {
  mode: {
    owner: { readable: true, executable: true, writable: true },
    group: { readable: true, executable: true, writable: false },
    others: { readable: true, executable: false, writable: false },
  },
  uid: 1000,
  gid: 500,
  atime: "3",
  ctime: "4",
  mtime: "5",
  size: "10",
  isDirectory: false,
  isFile: true,
  isSymlink: false,
};

describe("Object Codecs", () => {
  describe("File Item", () => {
    it("can encode RPC domain to Frontend domain", () => {
      const obj = E.encode({
        id: "node",
        name: "node",
        parent: "parent",
        stat,
        marked: false,
      });

      expect(obj.plain());
    });
  });
});
