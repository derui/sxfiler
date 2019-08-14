import bigInt from "big-integer";
import { Mode, createMode } from "./mode";
import { createCapability } from "./capability";

export type FileStat = {
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

export const sizeAsBigInt = function sizeAsBigInt(state: FileStat): bigInt.BigInteger {
  return bigInt(state.size);
};

/**
 * create FileStat
 * @param arg arguments of factory
 */
export const createFileStat = function createFileStat(arg: FactoryArg): FileStat {
  const { atime, ctime, mtime, mode, ...rest } = arg;
  return {
    ...rest,
    mode: createMode({
      group: createCapability(mode.group),
      owner: createCapability(mode.owner),
      others: createCapability(mode.others),
    }),
    atime: new Date(Number(atime)),
    ctime: new Date(Number(ctime)),
    mtime: new Date(Number(mtime)),
  };
};
