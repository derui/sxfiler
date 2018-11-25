// define type and operations for node
import { FileStat } from "./file-stat";

export interface Node {
  id: string;
  name: string;
  stat: FileStat;
  parentDirectory: string;
  linkPath?: string;
}
