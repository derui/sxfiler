import bigInt from "big-integer";

import { createFileStat } from "./file-stat";
import { Mode, modeOfBits } from "./mode";

describe("File Stat", () => {
  it("can get size as bigInt", () => {
    const stat = createFileStat({
      mode: modeOfBits(),
      uid: 1000,
      gid: 1000,
      atime: String(new Date("2019-01-22T01:02:03.123Z").getTime()),
      ctime: String(new Date("2019-01-22T01:02:04.123Z").getTime()),
      mtime: String(new Date("2019-01-22T01:02:05.123Z").getTime()),
      size: "1000",
      isDirectory: false,
      isFile: true,
      isSymlink: false,
    });

    expect(stat.sizeAsBigInt()).toEqual(bigInt(1000));
  });

  describe("createFileStat", () => {
    it("can convert mode from string", () => {
      const stat = createFileStat({
        mode: modeOfBits(0o764),
        uid: 1000,
        gid: 1000,
        atime: String(new Date("2019-01-22T01:02:03.123Z").getTime()),
        ctime: String(new Date("2019-01-22T01:02:04.123Z").getTime()),
        mtime: String(new Date("2019-01-22T01:02:05.123Z").getTime()),
        size: "100",
        isDirectory: false,
        isFile: true,
        isSymlink: false,
      });

      expect(stat.mode).toEqual({
        owner: { writable: true, readable: true, executable: true },
        group: { writable: true, readable: true, executable: false },
        others: { writable: false, readable: true, executable: false },
      });
    });

    it("can convert times as date", () => {
      const stat = createFileStat({
        mode: modeOfBits(0o764),
        uid: 1000,
        gid: 1000,
        atime: String(new Date("2019-01-22T01:02:03.123Z").getTime()),
        ctime: String(new Date("2019-01-22T01:02:04.123Z").getTime()),
        mtime: String(new Date("2019-01-22T01:02:05.123Z").getTime()),
        size: "100",
        isDirectory: false,
        isFile: true,
        isSymlink: false,
      });

      expect(stat.atime.toISOString()).toEqual("2019-01-22T01:02:03.123Z");
      expect(stat.ctime.toISOString()).toEqual("2019-01-22T01:02:04.123Z");
      expect(stat.mtime.toISOString()).toEqual("2019-01-22T01:02:05.123Z");
    });

    it("can get size as bigint", () => {
      const stat = createFileStat({
        mode: modeOfBits(0o764),
        uid: 1000,
        gid: 1000,
        atime: String(new Date("2019-01-22T01:02:03.123Z").getTime()),
        ctime: String(new Date("2019-01-22T01:02:04.123Z").getTime()),
        mtime: String(new Date("2019-01-22T01:02:05.123Z").getTime()),
        size: "12345678901234567890",
        isDirectory: false,
        isFile: true,
        isSymlink: false,
      });

      expect(stat.sizeAsBigInt.toString()).toEqual("12345678901234567890");
    });
  });
});
