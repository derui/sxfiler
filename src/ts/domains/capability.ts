// Capability with method
export type Capability = CapabilityObject & {
  /** Return new capability that allows to write */
  allowToWrite(): Capability;

  /** Return new capability that allows to read */
  allowToRead(): Capability;

  /** Return new capability that allows to execute */
  allowToExecute(): Capability;

  /** Return new capability that disallows to write */
  disallowToWrite(): Capability;

  /** Return new capability that disallows to read */
  disallowToRead(): Capability;

  /** Return new capability that disallows to execute */
  disallowToExecute(): Capability;

  /** Return plain CapabilityObject */
  plain(): CapabilityObject;
};

// Capability of a target
export type CapabilityObject = {
  readonly writable: boolean;
  readonly readable: boolean;
  readonly executable: boolean;
};

type CreateCapabilityArg = {
  writable: boolean;
  readable: boolean;
  executable: boolean;
};

function plain(this: Capability): CapabilityObject {
  return { writable: this.writable, readable: this.readable, executable: this.executable };
}

/**
   Create capability from argument
 */
export const createCapability = ({ writable, readable, executable }: CreateCapabilityArg): Capability => {
  return {
    writable,
    readable,
    executable,

    plain,

    /** Return new capability that allows to write */
    allowToWrite() {
      return { ...this, writable: true };
    },

    /** Return new capability that allows to read */
    allowToRead() {
      return { ...this, readable: true };
    },

    /** Return new capability that allows to execute */
    allowToExecute() {
      return { ...this, executable: true };
    },

    /** Return new capability that disallows to write */
    disallowToWrite() {
      return { ...this, writable: false };
    },

    /** Return new capability that disallows to read */
    disallowToRead() {
      return { ...this, readable: false };
    },

    /** Return new capability that disallows to execute */
    disallowToExecute() {
      return { ...this, executable: false };
    },
  };
};

/**
   Return new capability that have not any work.
 */
export const emptyCapability = (): Capability =>
  createCapability({ writable: false, readable: false, executable: false });

/**
   Return new capability that have all capability on all roles.
 */
export const fullCapability = (): Capability => createCapability({ writable: true, readable: true, executable: true });
