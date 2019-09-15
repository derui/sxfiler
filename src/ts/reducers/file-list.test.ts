// reducers for notification
import * as actions from "@/actions/filer";
import * as bookmarkActions from "@/actions/bookmark";
import { empty, State, Side, initialize } from "@/states/file-list";
import { reducer } from "./file-list";
import { createFiler, Direction } from "@/domains/filer";
import { createLocationHistory } from "@/domains/location-history";
import { createFileItem } from "@/domains/file-item";
import { createFileStat } from "@/domains/file-stat";
import { emptyMode } from "@/domains/mode";
import { createBookmark } from "@/domains/bookmark";

const history = createLocationHistory({ records: [], maxRecordNumber: 100 });
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

const node1 = createFileItem({
  id: "node1",
  name: "name1",
  stat,
  parentDirectory: "/",
  fullPath: "/name1",
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

const leftFiler = createFiler({
  id: "left",
  name: Side.Left,
  location: "loc",
  items: [node1, node2],
  currentCursorIndex: 0,
  history,
});

const rightFiler = createFiler({
  id: "right",
  name: Side.Right,
  location: "loc",
  items: [],
  currentCursorIndex: 0,
  history,
});

describe("reducers", () => {
  describe("filer state", () => {
    it("return new state that is updated with specified side and filer", () => {
      const state: State = empty();
      const ret = reducer(state, actions.update({ filer: leftFiler }));

      expect(ret.left).toEqual(leftFiler);
    });

    it("return new state with initialization payload", () => {
      const state: State = empty();
      const ret = reducer(state, actions.reload({ filers: [leftFiler, rightFiler] }));

      expect(ret.left).toEqual(leftFiler);
      expect(ret.right).toEqual(rightFiler);
    });

    it("change current side to other side", () => {
      let state: State = empty();
      state = { ...state, currentSide: Side.Left };

      const ret = reducer(state, actions.changeSide());
      expect(ret.currentSide).toEqual(Side.Right);
      expect(reducer(ret, actions.changeSide()).currentSide).toEqual(Side.Left);
    });

    it("use last current cursor index when loaded filer has same location", () => {
      let state: State = empty();
      state = initialize(state, { left: leftFiler.moveIndex(Direction.Down), right: rightFiler });
      state = { ...state, currentSide: Side.Left };

      const ret = reducer(state, actions.load({ filer: leftFiler }));
      expect(ret.left).toEqual(leftFiler.moveIndex(Direction.Down));
    });

    it("should select item when finder closed and selected", () => {
      let state: State = empty();
      state = initialize(state, { left: leftFiler, right: rightFiler });
      state = { ...state, currentSide: Side.Left };

      const ret = reducer(state, actions.select(Side.Left, "node2"));
      expect(ret.left!!.currentFileItem).toEqual(node2);
    });

    const bookmark = createBookmark({ id: "bookmark", path: "path", order: 1 });

    it("should add bookmark when get the action to register bookmark", () => {
      let state: State = empty();
      state = initialize(state, { left: leftFiler, right: rightFiler });
      state = { ...state, currentSide: Side.Left };

      const ret = reducer(state, bookmarkActions.registerBookmark(bookmark));
      expect(ret.bookmarks["bookmark"]).toEqual(bookmark);
    });

    it("should delete bookmark when get the action to delete bookmark", () => {
      let state: State = empty();
      state = initialize(state, { left: leftFiler, right: rightFiler });
      state = { ...state, currentSide: Side.Left };
      state = reducer(state, bookmarkActions.registerBookmark(bookmark));

      const ret = reducer(state, bookmarkActions.deleteBookmark(bookmark));
      expect(ret.bookmarks["bookmark"]).toBeUndefined;
    });

    it("should update bookmarks when get the action to update bookmark", () => {
      let state: State = empty();
      state = initialize(state, { left: leftFiler, right: rightFiler });
      state = { ...state, currentSide: Side.Left };

      const ret = reducer(state, bookmarkActions.updateBookmarks([bookmark]));
      expect(Object.values(ret.bookmarks)).toHaveLength(1);
      expect(ret.bookmarks["bookmark"]).toEqual(bookmark);
    });
  });
});
