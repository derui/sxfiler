// define filer state type and operations.
import { Node } from "./node";

export interface NodeMarker {
  node: Node;
  marked: boolean;
}

export enum Direction {
  Up = "up",
  Down = "down",
}

// Filer domain to handle nodes in the location
export class Filer {
  constructor(
    public readonly id: string,
    public readonly location: string,
    public readonly nodes: NodeMarker[],
    public readonly currentCursorIndex: number
  ) {}

  get currentNode(): Node {
    return this.nodes[this.currentCursorIndex].node;
  }

  /**
   * Get new instance of Filer that is moved by direction
   * @param direction direction to move index
   */
  public moveIndex(direction: Direction): Filer {
    let index = 0;

    switch (direction) {
      case Direction.Down:
        index = Math.min(this.nodes.length - 1, this.currentCursorIndex + 1);
        break;
      case Direction.Up:
        index = Math.max(0, this.currentCursorIndex - 1);
        break;
    }

    return new Filer(this.id, this.location, this.nodes, index);
  }

  /**
   * Mark specified node
   * @param nodeId
   */
  public toggleMark(nodeId: string): Filer {
    return new Filer(
      this.id,
      this.location,
      this.nodes.map(v => {
        if (v.node.id === nodeId) {
          return { node: v.node, marked: !v.marked };
        }
        return v;
      }),
      this.currentCursorIndex
    );
  }

  /**
   * @returns marked nodes
   */
  get markedNodes(): Node[] {
    return this.nodes.filter(v => v.marked).map(v => v.node);
  }
}
