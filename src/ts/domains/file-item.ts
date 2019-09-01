// define type and operations for node
import { FileStat } from "./file-stat";

export type FileItem = {
  readonly id: string;
  readonly name: string;
  readonly fullPath: string;
  readonly stat: FileStat;
  readonly parentDirectory: string;
  readonly marked: boolean;
  readonly linkPath?: string;
};

/**
 * create file item
 * @param args
 */
export const createFileItem = function createFileItem(args: FileItem): FileItem {
  return { ...args };
};
