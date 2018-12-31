import * as bigInt from "big-integer";

// define type and class of file stat
export interface Capability {
  readonly writable: boolean;
  readonly readable: boolean;
  readonly executable: boolean;
}

export interface Mode {
  readonly owner: Capability;
  readonly group: Capability;
  readonly others: Capability;
}

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

  get sizeAsBigInt(): bigInt.BigInteger {
    return bigInt(this.size);
  }
}
