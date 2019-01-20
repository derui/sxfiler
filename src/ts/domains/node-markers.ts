import { Node } from "./node";

export interface NodeMarker {
  node: Node;
  marked: boolean;
}

/**
 * A value object to hold nodes and restrict manipulation for it.
 */
export class NodeMarkers {
  constructor(private _value: NodeMarker[] = []) {}

  get value(): NodeMarker[] {
    return this._value.copyWithin(0, 0);
  }

  get markedNodes(): Node[] {
    return this._value.filter(v => v.marked).map(v => v.node);
  }

  public size() {
    return this._value.length;
  }

  public isEmpty() {
    return this._value.length === 0;
  }

  public toggleMarkById(nodeId: string) {
    return new NodeMarkers(
      this._value.map(v => {
        if (v.node.id === nodeId) {
          return { node: v.node, marked: !v.marked };
        }
        return v;
      })
    );
  }
}
