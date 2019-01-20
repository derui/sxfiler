// define type and operations for node
import { FileStat } from "./file-stat";

interface FactoryArg {
  id: string;
  name: string;
  stat: FileStat;
  parentDirectory: string;
  linkPath?: string;
}

/**
 * create node
 * @param args
 */
export function create(args: FactoryArg) {
  return new NodeImpl(args.id, args.name, args.stat, args.parentDirectory, args.linkPath);
}

export interface Node {
  readonly id: string;
  readonly name: string;
  readonly stat: FileStat;
  readonly parentDirectory: string;
  readonly linkPath?: string;
}

class NodeImpl implements Node {
  /**
   * create new Node
   * @param id id of node
   * @param name file or directory name of node
   * @param stat stats of node
   * @param parentDirectory parent directory of node
   * @param linkPath link target path if node is link
   */
  constructor(
    public readonly id: string,
    public readonly name: string,
    public readonly stat: FileStat,
    public readonly parentDirectory: string,
    public readonly linkPath?: string
  ) {}
}
