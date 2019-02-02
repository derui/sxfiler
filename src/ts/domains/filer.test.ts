import FileStatFactory from "./file-stat-factory";
import { Direction } from "./filer";
import Factory from "./filer-factory";
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

describe("Filer value object", () => {
  describe("getting current node", () => {
    it("should return if do not have any nodes", () => {
      const filer = Factory.create({ id: "id", location: "/loc", nodes: [] });

      expect(filer.currentNode).toBeNull();
    });

    it("should return current indexed node", () => {
      const node = createNode({
        id: "node1",
        name: "name",
        stat,
        parentDirectory: "/",
      });
      const filer = Factory.create({ id: "id", location: "/loc", nodes: [node] });

      expect(filer.currentNode).toEqual(node);
    });
  });

  describe("moving index", () => {
    it("can move index to next node that are sorted on create", () => {
      const node1 = createNode({ id: "node1", name: "name", stat, parentDirectory: "/" });
      const node2 = createNode({ id: "node2", name: "name2", stat, parentDirectory: "/" });
      const filer = Factory.create({ id: "id", location: "/loc", nodes: [node1, node2] });

      const nextFiler = filer.moveIndex(Direction.Down);
      expect(nextFiler.currentNode).toEqual(node2);
    });

    it("should not move to up direction if current is the first of nodes", () => {
      const node1 = createNode({ id: "node1", name: "name", stat, parentDirectory: "/" });
      const node2 = createNode({ id: "node2", name: "name2", stat, parentDirectory: "/" });
      const filer = Factory.create({ id: "id", location: "/loc", nodes: [node1, node2] });

      const nextFiler = filer.moveIndex(Direction.Up);
      expect(nextFiler.currentNode).toEqual(node1);
    });

    it("should not move to down direction if current is the last of nodes", () => {
      const node1 = createNode({ id: "node1", name: "name", stat, parentDirectory: "/" });
      const node2 = createNode({ id: "node2", name: "name2", stat, parentDirectory: "/" });
      const filer = Factory.create({ id: "id", location: "/loc", nodes: [node1, node2] });

      const nextFiler = filer.moveIndex(Direction.Down).moveIndex(Direction.Down);
      expect(nextFiler.currentNode).toEqual(node2);
    });
  });

  describe("toggling mark", () => {
    it("can enable mark with specified node", () => {
      const node1 = createNode({ id: "node1", name: "name", stat, parentDirectory: "/" });
      const node2 = createNode({ id: "node2", name: "name2", stat, parentDirectory: "/" });
      const filer = Factory.create({ id: "id", location: "/loc", nodes: [node1, node2] });

      const nextFiler = filer.toggleMark("node1");
      expect(nextFiler.markedNodes).toEqual([node1]);
    });

    it("should be mark disabled when call method twice to the node", () => {
      const node1 = createNode({ id: "node1", name: "name", stat, parentDirectory: "/" });
      const node2 = createNode({ id: "node2", name: "name2", stat, parentDirectory: "/" });
      const filer = Factory.create({ id: "id", location: "/loc", nodes: [node1, node2] });

      const nextFiler = filer.toggleMark("node1").toggleMark("node1");
      expect(nextFiler).not.toBe(filer);
      expect(nextFiler.markedNodes).toEqual([]);
    });
  });
});