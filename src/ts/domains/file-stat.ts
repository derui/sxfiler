import bigInt from "big-integer";

// Argument for Capability constructor
type CapabilityArg = {
  writable?: boolean;
  readable?: boolean;
  executable?: boolean;
}

// Capability of a target
export class Capability {
  readonly writable: boolean;
  readonly readable: boolean;
  readonly executable: boolean;

  constructor(arg: CapabilityArg = {}) {
    this.writable = arg.writable || false;
    this.readable = arg.readable || false;
    this.executable = arg.executable || false;
  }

  /** Return new capability that allows to write */
  allowToWrite(): Capability {
    return new Capability({ ...this, writable: true });
  }

  /** Return new capability that allows to read */
  allowToRead(): Capability {
    return new Capability({ ...this, readable: true });
  }

  /** Return new capability that allows to execute */
  allowToExecute(): Capability {
    return new Capability({ ...this, executable: true });
  }

  /** Return new capability that disallows to write */
  disallowToWrite() {
    return new Capability({ ...this, writable: false });
  }

  /** Return new capability that disallows to read */
  disallowToRead() {
    return new Capability({ ...this, readable: false });
  }

  /** Return new capability that disallows to execute */
  disallowToExecute() {
    return new Capability({ ...this, executable: false });
  }
}

// argument for Mode constructor
type ModeArg = {
  owner?: Capability;
  group?: Capability;
  others?: Capability;
}

// Mode of a FileStat
export class Mode {
  readonly owner: Capability;
  readonly group: Capability;
  readonly others: Capability;

  constructor(arg: ModeArg = {}) {
    this.owner = arg.owner || new Capability();
    this.group = arg.group || new Capability();
    this.others = arg.others || new Capability();
  }

  changeOwner(cap: Capability): Mode {
    return new Mode({ ...this, owner: cap });
  }

  changeGroup(cap: Capability): Mode {
    return new Mode({ ...this, group: cap });
  }

  changeOthers(cap: Capability): Mode {
    return new Mode({ ...this, others: cap });
  }
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
  ) { }

  /**
   *  get size of file as BigInt
   */
  get sizeAsBigInt(): bigInt.BigInteger {
    return bigInt(this.size);
  }
}
