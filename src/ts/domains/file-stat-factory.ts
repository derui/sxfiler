import { FileStat } from "./file-stat";
import { Capability } from "./capability";
import { Mode } from "./mode";

export interface FactoryArg {
  mode: string;
  uid: number;
  gid: number;
  atime: string;
  ctime: string;
  mtime: string;
  size: string;
  isDirectory: boolean;
  isFile: boolean;
  isSymlink: boolean;
}

/**
 * Convert from bits of capability to capability value object.
 * @param bits
 */
function bitsToCapability(bits: number): Capability {
  // tslint:disable:no-bitwise
  const writable = (bits & 0o2) === 0o2;
  return new Capability({
    writable,
    readable: (bits & 0o4) === 0o4,
    executable: (bits & 0o1) === 0o1,
  });
  // tslint:enable:no-bitwise
}

/**
 * convert string that contains capabilities
 * @param str mode string
 */
function toMode(str: string): Mode {
  const capabilities = Number(str);
  if (!Number.isInteger(capabilities)) {
    throw new Error(`Invalid number format: ${str}`);
  }

  // tslint:disable:no-bitwise
  const ownerBits = (capabilities & 0o700) >> 6;
  const groupBits = (capabilities & 0o70) >> 3;
  const othersBits = capabilities & 0o7;
  // tslint:enable:no-bitwise

  return new Mode({
    owner: bitsToCapability(ownerBits),
    group: bitsToCapability(groupBits),
    others: bitsToCapability(othersBits),
  });
}

// Factory of FileStat
export default class FileStatFactory {
  /**
   * create Filer
   * @param arg arguments of factory
   */
  public static create(arg: FactoryArg) {
    return new FileStat(
      toMode(arg.mode),
      arg.uid,
      arg.gid,
      new Date(arg.atime),
      new Date(arg.ctime),
      new Date(arg.mtime),
      arg.size,
      arg.isDirectory,
      arg.isFile,
      arg.isSymlink
    );
  }
}
