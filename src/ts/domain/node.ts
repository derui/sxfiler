// define type and operations for node
import { FileStat } from "./file-stat";

export class Node {
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
