// reducers for file list
import { Actions, ActionTypes } from "../actions";
import { empty, State, Side } from "../states/file-list";
import { Filer, createFiler } from "../domains/filer";

/**
 * The sub reducer to handle updateFiler action.
 */
const updateFiler = (state: State, payload: { filer: Filer }): State => {
  const { filer } = payload;

  switch (filer.name) {
    case Side.Left:
      return { ...state, left: filer };
    case Side.Right:
      return { ...state, right: filer };
    default:
      console.warn(`Unknown filer: ${filer.name}`);
      return state;
  }
};

/**
 * The sub reducer to handle loadFiler action.
 */
const updateFilerByServerState = (state: State, payload: { filer: Filer }): State => {
  const { filer } = payload;

  let updatedFiler = filer;
  switch (filer.name) {
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
    default:
      console.warn(`Unknown filer: ${filer.name}`);
      return state;
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

const reloadFiler = (state: State, filers: [Filer, Filer]): State => {
  const left = filers.find(v => v.name === Side.Left);
  const right = filers.find(v => v.name === Side.Right);

  return { ...state, left, right };
};

export const reducer = (state: State = empty(), action: Actions): State => {
  switch (action.type) {
    case ActionTypes.FILER_RELOAD:
      return reloadFiler(state, action.payload.filers);
    case ActionTypes.FILER_UPDATE:
      return updateFiler(state, action.payload);
    case ActionTypes.FILER_LOAD:
      return updateFilerByServerState(state, action.payload);
    case ActionTypes.FILER_CHANGE_SIDE:
      return { ...state, currentSide: moveToOtherSide(state.currentSide) };
  }
  return state;
};

export default reducer;
