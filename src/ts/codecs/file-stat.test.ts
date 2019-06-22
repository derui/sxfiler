import * as E from "./file-stat";
import { createFileStat } from "../domains/file-stat";
import { createMode } from "../domains/mode";
import { createCapability } from "../domains/capability";

describe("Object Codecs", () => {
  describe("File Stat", () => {
    it("can encode RPC domain to Frontend domain", () => {
      const obj = E.encode({
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
      });

      expect(obj.plain()).toEqual(
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
        }).plain()
      );
    });
  });
});
