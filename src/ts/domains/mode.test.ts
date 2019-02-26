import { Capability } from "./capability";
import { Mode } from "./mode";

describe("Domain", () => {
  describe("Mode", () => {
    it("should be able to create empty mode", () => {
      const mode = new Mode();
      const cap = new Capability({ writable: false, readable: false, executable: false });

      expect(mode.owner).toEqual(cap);
      expect(mode.group).toEqual(cap);
      expect(mode.others).toEqual(cap);
    });

    it("should be able to change capability of owner", () => {
      const mode = new Mode();
      const newMode = mode.changeOwner(new Capability({ writable: true }));

      expect(newMode.owner).toStrictEqual(new Capability({ writable: true }));
      expect(newMode.group).toEqual(mode.group);
      expect(newMode.others).toEqual(mode.others);
    });

    it("should be able to change capability of group", () => {
      const mode = new Mode();
      const newMode = mode.changeGroup(new Capability({ readable: true }));

      expect(newMode.group).toStrictEqual(new Capability({ readable: true }));
      expect(newMode.owner).toEqual(mode.group);
      expect(newMode.others).toEqual(mode.others);
    });

    it("should be able to change capability of others", () => {
      const mode = new Mode();
      const newMode = mode.changeOthers(new Capability({ executable: true }));

      expect(newMode.others).toStrictEqual(new Capability({ executable: true }));
      expect(newMode.owner).toEqual(mode.group);
      expect(newMode.group).toEqual(mode.others);
    });
  });
});
