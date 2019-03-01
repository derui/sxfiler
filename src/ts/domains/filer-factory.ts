import { Filer } from "./filer";
import { Node } from "./node";

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
      arg.nodes,
      0
    );
  }
}
