import { Filer } from "./filer";
import { Node } from "./node";
import { NodeMarkers } from "./node-markers";

export interface FactoryArg {
  id: string;
  location: string;
  nodes: Node[];
}

// Factory of filer
export default class FilerFactory {
  /**
   * create Filer
   * @param arg arguments of factory
   */
  public static create(arg: FactoryArg) {
    return new Filer(
      arg.id,
      arg.location,
      new NodeMarkers(
        arg.nodes.map(v => ({
          node: v,
          marked: false,
        }))
      ),
      0
    );
  }
}
