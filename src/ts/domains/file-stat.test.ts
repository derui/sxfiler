import bigInt from "big-integer";

import { createFileStat } from "./file-stat";
import { createMode, emptyMode } from "./mode";
import { createCapability, fullCapability, emptyCapability } from "./capability";

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

    expect(stat.sizeAsBigInt()).toEqual(bigInt(1000));
  });

  describe("createFileStat", () => {
    it("can convert times as date", () => {
      const stat = createFileStat({
        mode: createMode({
          owner: fullCapability(),
          group: fullCapability().disallowToExecute(),
          others: emptyCapability().allowToRead(),
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
          group: fullCapability().disallowToExecute(),
          others: emptyCapability().allowToRead(),
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

      expect(stat.sizeAsBigInt().toString()).toEqual("12345678901234567890");
    });
  });
});
