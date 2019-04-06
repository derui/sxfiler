// define filer state type and operations.
import { NodeObject } from "./node";

export enum Direction {
  Up = "up",
  Down = "down",
}

export type FactoryArg = {
  id: string;
  location: string;
  nodes: NodeObject[];
  currentCursorIndex: number;
};

export type FilerObject = {
  readonly id: string;
  readonly location: string;
  readonly nodes: NodeObject[];
  readonly currentCursorIndex: number;
};

export type Filer = FilerObject & {
  /**
   * the node object current marked
   */
  currentNode: NodeObject | undefined;

  /**
   * Get new instance of Filer that is moved by direction
   * @param direction direction to move index
   */
  moveIndex(direction: Direction): Filer;

  /**
   * @returns marked nodes
   */
  markedNodes: NodeObject[];
};

export const createFiler = ({ id, nodes, location, currentCursorIndex }: FactoryArg): Filer => {
  return {
    id,
    nodes,
    location,
    currentCursorIndex,
    get currentNode(): NodeObject | undefined {
      if (this.nodes.length === 0) {
        return undefined;
      }
      return this.nodes[this.currentCursorIndex];
    },

    /**
     * Get new instance of Filer that is moved by direction
     * @param direction direction to move index
     */
    moveIndex(direction: Direction): Filer {
      let index = 0;

      switch (direction) {
        case Direction.Down:
          index = Math.min(this.nodes.length - 1, this.currentCursorIndex + 1);
          break;
        case Direction.Up:
          index = Math.max(0, this.currentCursorIndex - 1);
          break;
      }

      return createFiler({ ...this, currentCursorIndex: index });
    },

    /**
     * @returns marked nodes
     */
    get markedNodes(): NodeObject[] {
      return this.nodes.filter(v => v.marked);
    },
  };
};
