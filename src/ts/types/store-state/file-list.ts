import { Node } from "../../domain/node";

interface Filer {
  id: string;
  location: string;
  nodes: Node[];
  selectedItemIndex: number;
}

export enum Side {
  Left = "left",
  Right = "right",
}

export interface StateArg {
  left?: Filer;
  right?: Filer;
  currentSide?: Side;
}

export class State {
  // filer of left side
  public left?: Filer;
  // filer of right side
  public right?: Filer;
  // current selected side
  public currentSide: Side;

  public constructor({ left, right, currentSide = Side.Left }: StateArg = {}) {
    this.left = left;
    this.right = right;
    this.currentSide = currentSide;
  }

  /**
   * Compare specific position is current or not.
   *
   * @param pos check position
   * @return state has same position
   */
  public isCurrent(pos: Side): boolean {
    return this.currentSide === pos;
  }

  /**
   * Get side swapped state.
   * @param state operation target
   * @return side swapped instance
   */
  public fellowPosition(): State {
    switch (this.currentSide) {
      case Side.Left:
        return new State(Object.assign({}, this, { currentSide: Side.Right }));
      case Side.Right:
        return new State(Object.assign({}, this, { currentSide: Side.Left }));
    }
  }
}
