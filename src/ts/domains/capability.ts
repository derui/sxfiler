enum CapabilityType {
  WRITE = "write",
  READ = "read",
  EXECUTE = "execute",
}

// Capability of a target
export type Capability = {
  readonly writable: boolean;
  readonly readable: boolean;
  readonly executable: boolean;
};

type CreateCapabilityArg = {
  writable: boolean;
  readable: boolean;
  executable: boolean;
};

/**
   Create capability from argument
 */
export const createCapability = ({ writable, readable, executable }: CreateCapabilityArg): Capability => {
  return {
    writable,
    readable,
    executable,
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

const allowTo = (typ: CapabilityType) => (capability: Capability) => {
  switch (typ) {
    case CapabilityType.WRITE:
      return createCapability({ ...capability, writable: true });
    case CapabilityType.READ:
      return createCapability({ ...capability, readable: true });
    case CapabilityType.EXECUTE:
      return createCapability({ ...capability, executable: true });
  }
};

const disallowTo = (typ: CapabilityType) => (capability: Capability) => {
  switch (typ) {
    case CapabilityType.WRITE:
      return createCapability({ ...capability, writable: false });
    case CapabilityType.READ:
      return createCapability({ ...capability, readable: false });
    case CapabilityType.EXECUTE:
      return createCapability({ ...capability, executable: false });
  }
};

export const allowToWrite = allowTo(CapabilityType.WRITE);
export const allowToRead = allowTo(CapabilityType.READ);
export const allowToExecute = allowTo(CapabilityType.EXECUTE);

export const disallowToWrite = disallowTo(CapabilityType.WRITE);
export const disallowToRead = disallowTo(CapabilityType.READ);
export const disallowToExecute = disallowTo(CapabilityType.EXECUTE);
