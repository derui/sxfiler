import { createNode } from "./node";
import { createFileStat } from "./file-stat";
import { emptyMode } from "./mode";

const stat = createFileStat({
  mode: emptyMode(),
  uid: 1000,
  gid: 1000,
  atime: "10",
  ctime: "11",
  mtime: "12",
  size: "100",
  isDirectory: false,
  isFile: true,
  isSymlink: false,
});

describe("Node value object", () => {
  it("can create the node", () => {
    const node = createNode({
      id: "id",
      name: "file",
      stat,
      parentDirectory: "parent",
      linkPath: "foo",
      marked: false,
    });

    expect(node.id).toEqual("id");
    expect(node.name).toEqual("file");
    expect(node.stat).toEqual(stat);
    expect(node.parentDirectory).toEqual("parent");
    expect(node.linkPath).toEqual("foo");
    expect(node.marked).toBeFalsy();
  });
});
