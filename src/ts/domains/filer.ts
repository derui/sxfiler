// define filer state type and operations.
import { Node } from "./node";
import { NodeMarkers } from "./node-markers";

export enum Direction {
  Up = "up",
  Down = "down",
}

// Filer domain to handle nodes in the location
export class Filer {
  constructor(
    public readonly id: string,
    public readonly location: string,
    public readonly nodes: NodeMarkers,
    public readonly currentCursorIndex: number
  ) {}

  get currentNode(): Node | null {
    if (this.nodes.isEmpty()) {
      return null;
    }
    return this.nodes.value[this.currentCursorIndex].node;
  }

  /**
   * Get new instance of Filer that is moved by direction
   * @param direction direction to move index
   */
  public moveIndex(direction: Direction): Filer {
    let index = 0;

    switch (direction) {
      case Direction.Down:
        index = Math.min(this.nodes.size() - 1, this.currentCursorIndex + 1);
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
    return new Filer(this.id, this.location, this.nodes.toggleMarkById(nodeId), this.currentCursorIndex);
  }

  /**
   * @returns marked nodes
   */
  get markedNodes(): Node[] {
    return this.nodes.markedNodes;
  }
}
