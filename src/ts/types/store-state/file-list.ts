import { Filer } from "../../domains/filer";

export enum Side {
  Left = "left",
  Right = "right",
}

export interface State {
  left?: Filer;
  // filer of right side
  right?: Filer;
  // current selected side
  currentSide: Side;
  // filer initialized or not
  initialized: boolean;
}

/** factory function create empty state */
export function empty(): State {
  return {
    currentSide: Side.Left,
    initialized: false,
  };
}

/**
 * Compare specific position is current or not.
 *
 * @param state state to operate
 * @param pos check position
 * @return state has same position
 */
export function isCurrent(state: State, pos: Side): boolean {
  return state.currentSide === pos;
}

/**
 * Get side swapped state.
 * @param state operation target
 * @return side swapped instance
 */
export function fellowPosition(state: State): State {
  switch (state.currentSide) {
    case Side.Left:
      return { ...state, currentSide: Side.Right };
    case Side.Right:
      return { ...state, currentSide: Side.Left };
  }
}
