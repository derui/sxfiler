import { Capability, capabilityOfBits, createCapability } from "./capability";

// Mode of a FileStat
export type Mode = {
  readonly owner: Capability;
  readonly group: Capability;
  readonly others: Capability;

  /**
     Get a new Mode that is changed owner's capability.
   */
  changeOwner(cap: Capability): Mode;

  /**
     Get a new Mode that is changed group's capability.
   */
  changeGroup(cap: Capability): Mode;

  /**
     Get a new Mode that is changed other's capability.
   */
  changeOthers(cap: Capability): Mode;
};

type CreateModeArg = {
  owner: Capability;
  group: Capability;
  others: Capability;
};

/**
   create mode from each capabilities
 */
export const createMode = ({ owner, group, others }: CreateModeArg): Mode => {
  return {
    owner,
    group,
    others,

    changeOwner(cap: Capability): Mode {
      return { ...this, owner: cap };
    },

    changeGroup(cap: Capability): Mode {
      return { ...this, group: cap };
    },

    changeOthers(cap: Capability): Mode {
      return { ...this, others: cap };
    },
  };
};

export const emptyMode= (): Mode => createMode({owner: })


/**
 * convert number to mode that contains capabilities
 * @param capabilities mode bits. default is 0o777
 */
export const modeOfBits = (capabilities: number = 0o777): Mode => {
  const ownerBits = (capabilities & 0o700) >> 6;
  const groupBits = (capabilities & 0o70) >> 3;
  const othersBits = capabilities & 0o7;

  return createMode({
    owner: capabilityOfBits(ownerBits),
    group: capabilityOfBits(groupBits),
    others: capabilityOfBits(othersBits),
  });
};
