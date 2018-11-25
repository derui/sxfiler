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
  constructor(private id: string, private location: string, private nodes: NodeMarker[]) {}

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
      })
    );
  }

  /**
   * @returns marked nodes
   */
  get markedNodes(): Node[] {
    return this.nodes.filter(v => v.marked).map(v => v.node);
  }
}
