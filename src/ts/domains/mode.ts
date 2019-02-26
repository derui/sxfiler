import { Capability } from "./capability";

// argument for Mode constructor
interface ModeArg {
  owner?: Capability;
  group?: Capability;
  others?: Capability;
}

// Mode of a FileStat
export class Mode {
  public readonly owner: Capability;
  public readonly group: Capability;
  public readonly others: Capability;

  constructor(arg: ModeArg = {}) {
    this.owner = arg.owner || new Capability();
    this.group = arg.group || new Capability();
    this.others = arg.others || new Capability();
  }

  public changeOwner(cap: Capability): Mode {
    return new Mode({ ...this, owner: cap });
  }

  public changeGroup(cap: Capability): Mode {
    return new Mode({ ...this, group: cap });
  }

  public changeOthers(cap: Capability): Mode {
    return new Mode({ ...this, others: cap });
  }
}
