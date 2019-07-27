import { Capability, emptyCapability } from "./capability";

// A plain mode object
export type Mode = {
  readonly owner: Capability;
  readonly group: Capability;
  readonly others: Capability;
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
  };
};

/**
   Get a new mode that do not have any capability.
 */
export const emptyMode = (): Mode =>
  createMode({ owner: emptyCapability(), group: emptyCapability(), others: emptyCapability() });

/**
   change capability of owner
 */
export const changeOwner = (cap: Capability) => (mode: Mode): Mode => createMode({ ...mode, owner: cap });

/**
   change capability of group
 */
export const changeGroup = (cap: Capability) => (mode: Mode): Mode => createMode({ ...mode, group: cap });

/**
   change capability of others
 */
export const changeOthers = (cap: Capability) => (mode: Mode): Mode => createMode({ ...mode, others: cap });
