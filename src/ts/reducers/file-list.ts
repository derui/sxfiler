// reducers for file list
import { Actions, ActionTypes } from "@/actions";
import { empty, State, Side } from "@/states/file-list";
import { Filer, createFiler, selectItemById } from "@/domains/filer";

/**
 * The sub reducer to handle updateFiler action.
 */
const updateFiler = function updateFiler(state: State, payload: { filer: Filer }): State {
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
const updateFilerByServerState = function updateFilerByServerState(state: State, payload: { filer: Filer }): State {
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

const moveToOtherSide = function moveToOtherSide(current: Side): Side {
  switch (current) {
    case Side.Left:
      return Side.Right;
    case Side.Right:
      return Side.Left;
  }
};

const reloadFiler = function reloadFiler(state: State, filers: [Filer, Filer]): State {
  const left = filers.find(v => v.name === Side.Left);
  const right = filers.find(v => v.name === Side.Right);

  return { ...state, left, right };
};

const selectItem = function selectItemWhenSelectedByFinder(state: State, side: Side, itemId: string) {
  switch (side) {
    case Side.Left:
      if (!state.left) {
        return state;
      }
      return { ...state, left: selectItemById(itemId)(state.left) };
    case Side.Right:
      if (!state.right) {
        return state;
      }
      return { ...state, right: selectItemById(itemId)(state.right) };
  }
};

export const reducer = function reducer(state: State = empty(), action: Actions): State {
  switch (action.type) {
    case ActionTypes.FILER_RELOAD:
      return reloadFiler(state, action.payload.filers);
    case ActionTypes.FILER_UPDATE:
      return updateFiler(state, action.payload);
    case ActionTypes.FILER_LOAD:
      return updateFilerByServerState(state, action.payload);
    case ActionTypes.FILER_CHANGE_SIDE:
      return { ...state, currentSide: moveToOtherSide(state.currentSide) };
    case ActionTypes.FILER_SELECT:
      return selectItem(state, action.side, action.itemId);
  }
  return state;
};
