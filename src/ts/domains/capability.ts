// Argument for Capability constructor
interface CapabilityArg {
  writable?: boolean;
  readable?: boolean;
  executable?: boolean;
}

// Capability of a target
export class Capability {
  public readonly writable: boolean;
  public readonly readable: boolean;
  public readonly executable: boolean;

  constructor(arg: CapabilityArg = {}) {
    this.writable = arg.writable || false;
    this.readable = arg.readable || false;
    this.executable = arg.executable || false;
  }

  /** Return new capability that allows to write */
  public allowToWrite(): Capability {
    return new Capability({ ...this, writable: true });
  }

  /** Return new capability that allows to read */
  public allowToRead(): Capability {
    return new Capability({ ...this, readable: true });
  }

  /** Return new capability that allows to execute */
  public allowToExecute(): Capability {
    return new Capability({ ...this, executable: true });
  }

  /** Return new capability that disallows to write */
  public disallowToWrite() {
    return new Capability({ ...this, writable: false });
  }

  /** Return new capability that disallows to read */
  public disallowToRead() {
    return new Capability({ ...this, readable: false });
  }

  /** Return new capability that disallows to execute */
  public disallowToExecute() {
    return new Capability({ ...this, executable: false });
  }
}
