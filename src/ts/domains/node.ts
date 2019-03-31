// define type and operations for node
import { FileStat } from "./file-stat";

interface FactoryArg {
  id: string;
  name: string;
  stat: FileStat;
  parentDirectory: string;
  linkPath?: string;
  marked: boolean;
}

/**
 * create node
 * @param args
 */
export function createNode(args: FactoryArg): NodeObject {
  return { ...args };
}

export type NodeObject = {
  readonly id: string;
  readonly name: string;
  readonly stat: FileStat;
  readonly parentDirectory: string;
  readonly marked: boolean;
  readonly linkPath?: string;
};
