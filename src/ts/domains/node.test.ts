import FileStatFactory from "./file-stat-factory";
import { create as createNode } from "./node";

const stat = FileStatFactory.create({
  mode: String(0o777),
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

    expect(node.id).toBe("id");
    expect(node.name).toBe("file");
    expect(node.stat).toBe(stat);
    expect(node.parentDirectory).toBe("parent");
    expect(node.linkPath).toBe("foo");
    expect(node.marked).toBeFalsy();
  });
});
