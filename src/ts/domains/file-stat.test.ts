import bigInt from "big-integer";

import { createFileStat, sizeAsBigInt } from "./file-stat";
import { createMode, emptyMode } from "./mode";
import { fullCapability, emptyCapability, disallowToExecute, allowToRead } from "./capability";

describe("File Stat", () => {
  it("can get size as bigInt", () => {
    const stat = createFileStat({
      mode: emptyMode(),
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

    expect(sizeAsBigInt(stat)).toEqual(bigInt(1000));
  });

  describe("createFileStat", () => {
    it("can convert times as date", () => {
      const stat = createFileStat({
        mode: createMode({
          owner: fullCapability(),
          group: disallowToExecute(fullCapability()),
          others: allowToRead(emptyCapability()),
        }),
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
        mode: createMode({
          owner: fullCapability(),
          group: disallowToExecute(fullCapability()),
          others: allowToRead(emptyCapability()),
        }),
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

      expect(sizeAsBigInt(stat).toString()).toEqual("12345678901234567890");
    });
  });
});
