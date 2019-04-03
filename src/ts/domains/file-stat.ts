import bigInt from "big-integer";
import { Mode } from "./mode";

// object mode of file
export type FileStat = FileStatObject & {
  /**
   *  get size of file as BigInt
   */
  sizeAsBigInt(): bigInt.BigInteger;
};

export type FileStatObject = {
  readonly mode: Mode;
  readonly uid: number;
  readonly gid: number;
  readonly atime: Date;
  readonly ctime: Date;
  readonly mtime: Date;
  readonly size: string;
  readonly isDirectory: boolean;
  readonly isFile: boolean;
  readonly isSymlink: boolean;
};

export type FactoryArg = {
  mode: Mode;
  uid: number;
  gid: number;
  atime: string;
  ctime: string;
  mtime: string;
  size: string;
  isDirectory: boolean;
  isFile: boolean;
  isSymlink: boolean;
};

/**
 * create FileStat
 * @param arg arguments of factory
 */
export const createFileStat = (arg: FactoryArg): FileStat => {
  const { atime, ctime, mtime, ...rest } = arg;
  return {
    ...rest,
    atime: new Date(Number(arg.atime)),
    ctime: new Date(Number(arg.ctime)),
    mtime: new Date(Number(arg.mtime)),
    sizeAsBigInt(): bigInt.BigInteger {
      return bigInt(this.size);
    },
  };
};
