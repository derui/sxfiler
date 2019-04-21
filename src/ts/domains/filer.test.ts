import { Direction, createFiler } from "./filer";
import { createNode } from "./node";
import { emptyMode } from "./mode";
import { createFileStat } from "./file-stat";

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

describe("Filer domain", () => {
  describe("comparable", () => {
    it("compare with filers that has same content", () => {
      const filer = createFiler({ id: "id", location: "/loc", nodes: [], currentCursorIndex: 0 });
      const filer2 = createFiler({ id: "id", location: "/loc", nodes: [], currentCursorIndex: 0 });

      expect(filer).toEqual(filer2);
    });
  });

  describe("getting current node", () => {
    it("should return if do not have any nodes", () => {
      const filer = createFiler({ id: "id", location: "/loc", nodes: [], currentCursorIndex: 0 });

      expect(filer.currentNode).toBeUndefined();
    });

    it("should return current indexed node", () => {
      const node = createNode({
        id: "node1",
        name: "name",
        stat,
        parentDirectory: "/",
        marked: false,
      });
      const filer = createFiler({ id: "id", location: "/loc", nodes: [node], currentCursorIndex: 0 });

      expect(filer.currentNode).toEqual(node);
    });
  });

  describe("moving index", () => {
    it("can move index to next node that are sorted on create", () => {
      const node1 = createNode({ id: "node1", name: "name", stat, parentDirectory: "/", marked: false });
      const node2 = createNode({ id: "node2", name: "name2", stat, parentDirectory: "/", marked: false });
      const filer = createFiler({ id: "id", location: "/loc", nodes: [node1, node2], currentCursorIndex: 0 });

      const nextFiler = filer.moveIndex(Direction.Down);
      expect(nextFiler.currentNode).toEqual(node2);
    });

    it("should not move to up direction if current is the first of nodes", () => {
      const node1 = createNode({ id: "node1", name: "name", stat, parentDirectory: "/", marked: false });
      const node2 = createNode({ id: "node2", name: "name2", stat, parentDirectory: "/", marked: false });
      const filer = createFiler({ id: "id", location: "/loc", nodes: [node1, node2], currentCursorIndex: 0 });

      const nextFiler = filer.moveIndex(Direction.Up);
      expect(nextFiler.currentNode).toEqual(node1);
    });

    it("should not move to down direction if current is the last of nodes", () => {
      const node1 = createNode({ id: "node1", name: "name", stat, parentDirectory: "/", marked: false });
      const node2 = createNode({ id: "node2", name: "name2", stat, parentDirectory: "/", marked: false });
      const filer = createFiler({ id: "id", location: "/loc", nodes: [node1, node2], currentCursorIndex: 0 });

      const nextFiler = filer.moveIndex(Direction.Down).moveIndex(Direction.Down);
      expect(nextFiler.currentNode).toEqual(node2);
    });
  });

  describe("property", () => {
    it("should be able to retrieve marked node ", () => {
      const node1 = createNode({ id: "node1", name: "name", stat, parentDirectory: "/", marked: true });
      const node2 = createNode({ id: "node2", name: "name2", stat, parentDirectory: "/", marked: false });
      const filer = createFiler({ id: "id", location: "/loc", nodes: [node1, node2], currentCursorIndex: 0 });

      expect(filer.markedNodes).toEqual([node1]);
    });
  });
});
