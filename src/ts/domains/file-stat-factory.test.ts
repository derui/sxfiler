import Factory from "./file-stat-factory";

describe("file stat factory", () => {
  it("can convert mode from string", () => {
    const stat = Factory.create({
      mode: String(0o764),
      uid: 1000,
      gid: 1000,
      atime: "2019-01-22T01:02:03.123Z",
      ctime: "2019-01-22T01:02:04.123Z",
      mtime: "2019-01-22T01:02:05.123Z",
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
    const stat = Factory.create({
      mode: String(0o764),
      uid: 1000,
      gid: 1000,
      atime: "2019-01-22T01:02:03.123Z",
      ctime: "2019-01-22T01:02:04.123Z",
      mtime: "2019-01-22T01:02:05.123Z",
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
    const stat = Factory.create({
      mode: String(0o764),
      uid: 1000,
      gid: 1000,
      atime: "2019-01-22T01:02:03.123Z",
      ctime: "2019-01-22T01:02:04.123Z",
      mtime: "2019-01-22T01:02:05.123Z",
      size: "12345678901234567890",
      isDirectory: false,
      isFile: true,
      isSymlink: false,
    });

    expect(stat.sizeAsBigInt.toString()).toEqual("12345678901234567890");
  });
});
