// reducers for notification
import { actions } from "@/actions/filer";
import { empty, State, Side, initialize } from "@/states/file-list";
import { reducer } from "./file-list";
import { createFiler, Direction } from "@/domains/filer";
import { createLocationHistory } from "@/domains/location-history";

const history = createLocationHistory({ records: [], maxRecordNumber: 100 });
const leftFiler = createFiler({
  id: "left",
  name: Side.Left,
  location: "loc",
  items: [],
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
  });
});
