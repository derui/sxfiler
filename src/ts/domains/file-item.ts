// define type and operations for node
import { FileStat, FileStatObject } from "./file-stat";

interface FactoryArg {
  id: string;
  name: string;
  stat: FileStat;
  parentDirectory: string;
  linkPath?: string;
  marked: boolean;
}

function plain(this: FileItem): FileItemObject {
  const {
    plain, // eslint-disable-line @typescript-eslint/no-unused-vars
    stat,
    ...rest
  } = this;

  return { ...rest, stat: stat.plain() };
}

/**
 * create node
 * @param args
 */
export const createFileItem = (args: FactoryArg): FileItem => {
  return { ...args, plain };
};

export type FileItemObject = {
  readonly id: string;
  readonly name: string;
  readonly stat: FileStatObject;
  readonly parentDirectory: string;
  readonly marked: boolean;
  readonly linkPath?: string;
};

export type FileItem = FileItemObject & {
  stat: FileStat;
  plain(): FileItemObject;
};
