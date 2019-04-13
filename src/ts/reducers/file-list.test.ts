// reducers for notification
import { actions } from "../actions/filer";
import { empty, State, Side } from "../states/file-list";
import reducer from "./file-list";
import { createFiler } from "../domains/filer";

describe("reducers", () => {
  describe("filer state", () => {
    it("return new state that is updated with specified side and filer", () => {
      const state: State = empty();
      const filer = createFiler({
        id: "id",
        location: "loc",
        nodes: [],
        currentCursorIndex: 0,
      });

      const ret = reducer(state, actions.update({ side: Side.Left, filer: filer }));

      expect(ret.left).toEqual(filer);
    });

    it("return new state with initialization payload", () => {
      const state: State = empty();
      const leftFiler = createFiler({
        id: "left",
        location: "loc",
        nodes: [],
        currentCursorIndex: 0,
      });

      const rightFiler = createFiler({
        id: "right",
        location: "loc",
        nodes: [],
        currentCursorIndex: 0,
      });

      const ret = reducer(state, actions.initialize({ left: leftFiler, right: rightFiler }));

      expect(ret.initialized).toBeTruthy();
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
  });
});
