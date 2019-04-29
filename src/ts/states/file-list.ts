import { Filer } from "../domains/filer";
import { NodeObject } from "../domains/node";

export enum Side {
  Left = "left",
  Right = "right",
}

export type State = {
  left?: Filer;
  // filer of right side
  right?: Filer;
  // current selected side
  currentSide: Side;
  // filer initialized or not
  initialized: boolean;
};

/** factory function create empty state */
export const empty = (): State => {
  return {
    currentSide: Side.Left,
    initialized: false,
  };
};

/**
   Initialize a state with filers
 */
export const initialize = (state: State, { left, right }: { left: Filer; right: Filer }): State => {
  return {
    ...state,
    initialized: true,
    left,
    right,
  };
};

/**
 * Get the filer on the side
 *
 * @param state The state of file list
 * @param side the side to get filer from state
 */
export const filerOnSide = (state: State, side: Side): Filer | undefined => {
  switch (side) {
    case Side.Left:
      return state.left;
    case Side.Right:
      return state.right;
  }
};

/**
 * Compare specific position is current or not.
 *
 * @param state state to operate
 * @param pos check position
 * @return state has same position
 */
export const isCurrent = (state: State, pos: Side): boolean => {
  return state.currentSide === pos;
};

/**
 * Get side swapped state.
 * @param state operation target
 * @return side swapped instance
 */
export const fellowPosition = (state: State): State => {
  switch (state.currentSide) {
    case Side.Left:
      return { ...state, currentSide: Side.Right };
    case Side.Right:
      return { ...state, currentSide: Side.Left };
  }
};

/**
 * Get current focusing node
 *
 * @param state The state of file list
 */
export const currentFocusingNode = (state: State): NodeObject | undefined => {
  if (!state.initialized || !state.left || !state.right) {
    return undefined;
  }

  const side = state.currentSide;

  switch (side) {
    case Side.Left:
      return state.left.currentNode;
    case Side.Right:
      return state.right.currentNode;
  }
};
