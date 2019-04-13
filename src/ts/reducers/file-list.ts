// reducers for file list
import { Actions } from "../actions";
import { empty, State, Side } from "../states/file-list";
import { ActionTypes } from "../actions/filer";
import { Filer } from "../domains/filer";

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
    case ActionTypes.changeSide:
      return { ...state, currentSide: moveToOtherSide(state.currentSide) };
  }
  return state;
};

export default reducer;
