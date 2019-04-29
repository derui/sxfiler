// reducers for file list
import { Actions } from "../actions";
import { empty, State, Side } from "../states/file-list";
import { ActionTypes } from "../actions/filer";
import { Filer, createFiler } from "../domains/filer";

/**
 * The sub reducer to handle updateFiler action.
 */
const updateFiler = (state: State, payload: { side: Side; filer: Filer }): State => {
  const { side, filer } = payload;

  switch (side) {
    case Side.Left:
      return { ...state, left: filer };
    case Side.Right:
      return { ...state, right: filer };
  }
};

/**
 * The sub reducer to handle loadFiler action.
 */
const updateFilerByServerState = (state: State, payload: { side: Side; filer: Filer }): State => {
  const { side, filer } = payload;

  let updatedFiler = filer;
  switch (side) {
    case Side.Left:
      if (state.left && state.left.location === filer.location) {
        updatedFiler = createFiler({
          ...filer,
          currentCursorIndex: state.left.currentCursorIndex,
        });
      }
      return { ...state, left: updatedFiler };
    case Side.Right:
      if (state.right && state.right.location === filer.location) {
        updatedFiler = createFiler({
          ...filer,
          currentCursorIndex: state.right.currentCursorIndex,
        });
      }
      return { ...state, right: updatedFiler };
  }
};

const moveToOtherSide = (current: Side): Side => {
  switch (current) {
    case Side.Left:
      return Side.Right;
    case Side.Right:
      return Side.Left;
  }
};

export const reducer = (state: State = empty(), action: Actions): State => {
  switch (action.type) {
    case ActionTypes.initialize:
      return {
        ...state,
        initialized: true,
        left: action.payload.left,
        right: action.payload.right,
      };
    case ActionTypes.updateFiler:
      return updateFiler(state, action.payload);
    case ActionTypes.loadFiler:
      return updateFilerByServerState(state, action.payload);
    case ActionTypes.changeSide:
      return { ...state, currentSide: moveToOtherSide(state.currentSide) };
  }
  return state;
};

export default reducer;
