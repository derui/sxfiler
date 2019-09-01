import { Direction, createFiler } from "./filer";
import { createFileItem } from "./file-item";
import { emptyMode } from "./mode";
import { createFileStat } from "./file-stat";
import { createLocationHistory } from "./location-history";

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
  const history = createLocationHistory({ records: [], maxRecordNumber: 100 });

  describe("factory", () => {
    it("avoid to overlap current cursor index", () => {
      const node1 = createFileItem({
        id: "node1",
        name: "name",
        stat,
        fullPath: "/name",
        parentDirectory: "/",
        marked: false,
      });
      const node2 = createFileItem({
        id: "node2",
        name: "name",
        stat,
        fullPath: "/name",
        parentDirectory: "/",
        marked: false,
      });

      const filer = createFiler({
        id: "id",
        name: "name",
        location: "/loc",
        items: [node1, node2],
        currentCursorIndex: 1,
        history,
      });
      const filer2 = createFiler({ ...filer, items: [node1], currentCursorIndex: 2 });

      expect(filer2.currentFileItem).toEqual(node1);
    });
  });
  describe("comparable", () => {
    it("compare with filers that has same content", () => {
      const filer = createFiler({
        id: "id",
        name: "name",
        location: "/loc",
        items: [],
        currentCursorIndex: 0,
        history,
      });
      const filer2 = createFiler({
        id: "id",
        name: "name",
        location: "/loc",
        items: [],
        currentCursorIndex: 0,
        history,
      });

      expect(filer).toEqual(filer2);
    });
  });

  describe("getting current node", () => {
    it("should return if do not have any nodes", () => {
      const filer = createFiler({
        id: "id",
        name: "name",
        location: "/loc",
        items: [],
        currentCursorIndex: 0,
        history,
      });

      expect(filer.currentFileItem).toBeUndefined();
    });

    it("should return current indexed node", () => {
      const node = createFileItem({
        id: "node1",
        name: "name",
        stat,
        fullPath: "/name",
        parentDirectory: "/",
        marked: false,
      });
      const filer = createFiler({
        id: "id",
        name: "name",
        location: "/loc",
        items: [node],
        currentCursorIndex: 0,
        history,
      });

      expect(filer.currentFileItem).toEqual(node);
    });
  });

  describe("moving index", () => {
    const node1 = createFileItem({
      id: "node1",
      name: "name",
      stat,
      parentDirectory: "/",
      fullPath: "/name",
      marked: false,
    });
    const node2 = createFileItem({
      id: "node2",
      name: "name2",
      stat,
      parentDirectory: "/",
      fullPath: "/name2",
      marked: false,
    });
    it("can move index to next node that are sorted on create", () => {
      const filer = createFiler({
        id: "id",
        name: "name",
        location: "/loc",
        items: [node1, node2],
        currentCursorIndex: 0,
        history,
      });

      const nextFiler = filer.moveIndex(Direction.Down);
      expect(nextFiler.currentFileItem).toEqual(node2);
    });

    it("can move index to next node that are sorted on create", () => {
      const filer = createFiler({
        id: "id",
        name: "name",
        location: "/loc",
        items: [node1, node2],
        currentCursorIndex: 0,
        history,
      });

      const nextFiler = filer.moveIndex(Direction.Down);
      expect(nextFiler.currentCursorIndex).toEqual(1);
    });

    it("should not move to up direction if current is the first of nodes", () => {
      const filer = createFiler({
        id: "id",
        name: "name",
        location: "/loc",
        items: [node1, node2],
        currentCursorIndex: 0,
        history,
      });

      const nextFiler = filer.moveIndex(Direction.Up);
      expect(nextFiler.currentFileItem).toEqual(node1);
    });

    it("should not move to down direction if current is the last of nodes", () => {
      const filer = createFiler({
        id: "id",
        name: "name",
        location: "/loc",
        items: [node1, node2],
        currentCursorIndex: 0,
        history,
      });

      const nextFiler = filer.moveIndex(Direction.Down).moveIndex(Direction.Down);
      expect(nextFiler.currentFileItem).toEqual(node2);
    });
  });

  describe("property", () => {
    const node1Data = { id: "node1", name: "name", stat, parentDirectory: "/", marked: false, fullPath: "/name" };
    const node2Data = { id: "node2", name: "name2", stat, parentDirectory: "/", marked: false, fullPath: "/name2" };

    it("should be able to retrieve marked node ", () => {
      const node1 = createFileItem({ ...node1Data, marked: true });
      const node2 = createFileItem({ ...node2Data, marked: false });
      const filer = createFiler({
        id: "id",
        name: "name",
        location: "/loc",
        items: [node1, node2],
        currentCursorIndex: 0,
        history,
      });

      expect(filer.markedItems).toEqual([node1]);
    });

    it("return only current focused node", () => {
      const node1 = createFileItem({ ...node1Data });
      const node2 = createFileItem({ ...node2Data });
      const filer = createFiler({
        id: "id",
        name: "name",
        location: "/loc",
        items: [node1, node2],
        currentCursorIndex: 1,
        history,
      });

      expect(filer.markedItems).toEqual([node2]);
    });
  });
});
