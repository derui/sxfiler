import bigInt from "big-integer";
import { Mode, ModeObject, createMode } from "./mode";
import { createCapability } from "./capability";

// object mode of file
export type FileStat = FileStatObject & {
  mode: Mode;
  /**
   *  get size of file as BigInt
   */
  sizeAsBigInt(): bigInt.BigInteger;

  /**
   * Get POJO of this object
   */
  plain(): FileStatObject;
};

export type FileStatObject = {
  readonly mode: ModeObject;
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
  mode: ModeObject;
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
    sizeAsBigInt(): bigInt.BigInteger {
      return bigInt(this.size);
    },
    plain(): FileStatObject {
      const {
        sizeAsBigInt, // eslint-disable-line @typescript-eslint/no-unused-vars
        mode,
        plain, // eslint-disable-line @typescript-eslint/no-unused-vars
        ...rest
      } = this;
      return { ...rest, mode: mode.plain() };
    },
  };
};
