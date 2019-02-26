import bigInt from "big-integer";
import { Mode } from "./mode";

// information object of file
export class FileStat {
  constructor(
    public readonly mode: Mode,
    public readonly uid: number,
    public readonly gid: number,
    public readonly atime: Date,
    public readonly ctime: Date,
    public readonly mtime: Date,
    public readonly size: string,
    public readonly isDirectory: boolean,
    public readonly isFile: boolean,
    public readonly isSymlink: boolean
  ) {}

  /**
   *  get size of file as BigInt
   */
  get sizeAsBigInt(): bigInt.BigInteger {
    return bigInt(this.size);
  }
}
