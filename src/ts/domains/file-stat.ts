import bigInt from "big-integer";
import { Mode } from "./mode";

// object mode of file
export class FileStat {
  public readonly mode: Mode;
  public readonly uid: number;
  public readonly gid: number;
  public readonly atime: Date;
  public readonly ctime: Date;
  public readonly mtime: Date;
  public readonly size: string;
  public readonly isDirectory: boolean;
  public readonly isFile: boolean;
  public readonly isSymlink: boolean;

  public constructor(
    mode: Mode,
    uid: number,
    gid: number,
    atime: Date,
    ctime: Date,
    mtime: Date,
    size: string,
    isDirectory: boolean,
    isFile: boolean,
    isSymlink: boolean
  ) {
    this.mode = mode;
    this.uid = uid;
    this.gid = gid;
    this.atime = atime;
    this.ctime = ctime;
    this.mtime = mtime;
    this.size = size;
    this.isDirectory = isDirectory;
    this.isFile = isFile;
    this.isSymlink = isSymlink;
  }

  /**
   *  get size of file as BigInt
   */
  public get sizeAsBigInt(): bigInt.BigInteger {
    return bigInt(this.size);
  }
}
