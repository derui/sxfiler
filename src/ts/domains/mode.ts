import { Capability, emptyCapability, CapabilityObject } from "./capability";

// A plain mode object
export type ModeObject = {
  readonly owner: CapabilityObject;
  readonly group: CapabilityObject;
  readonly others: CapabilityObject;
};

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

  /**
     Plain version mode
   */

  plain(): ModeObject;
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

    plain() {
      return {
        owner: this.owner.plain(),
        group: this.group.plain(),
        others: this.others.plain(),
      };
    },

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

/**
   Get a new mode that do not have any capability.
 */
export const emptyMode = (): Mode =>
  createMode({ owner: emptyCapability(), group: emptyCapability(), others: emptyCapability() });
