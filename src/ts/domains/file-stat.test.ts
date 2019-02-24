import { Mode, Capability } from "./file-stat";

describe('File Stat', () => {
  describe('Mode', () => {
    it('should be able to create empty mode', () => {
      const mode = new Mode();
      const cap = new Capability({ writable: false, readable: false, executable: false });

      expect(mode.owner).toEqual(cap);
      expect(mode.group).toEqual(cap);
      expect(mode.others).toEqual(cap);
    });

    it('should be able to change capability of owner', () => {
      const mode = new Mode();
      const newMode = mode.changeOwner(new Capability({ writable: true }));

      expect(newMode.owner).toStrictEqual(new Capability({ writable: true }));
      expect(newMode.group).toEqual(mode.group);
      expect(newMode.others).toEqual(mode.others);
    });

    it('should be able to change capability of group', () => {
      const mode = new Mode();
      const newMode = mode.changeGroup(new Capability({ readable: true }));

      expect(newMode.group).toStrictEqual(new Capability({ readable: true }));
      expect(newMode.owner).toEqual(mode.group);
      expect(newMode.others).toEqual(mode.others);
    });

    it('should be able to change capability of others', () => {
      const mode = new Mode();
      const newMode = mode.changeOthers(new Capability({ executable: true }));

      expect(newMode.others).toStrictEqual(new Capability({ executable: true }));
      expect(newMode.owner).toEqual(mode.group);
      expect(newMode.group).toEqual(mode.others);
    });
  });

  describe('Capability', () => {
    it('should be disabled all permission for initial state', () => {
      const capability = new Capability();

      expect(capability.readable).toBeFalsy();
      expect(capability.writable).toBeFalsy();
      expect(capability.executable).toBeFalsy();
    });

    it('allows to allow some permission', () => {
      const capability = new Capability();

      const newCap = capability.allowToExecute().allowToRead().allowToWrite();

      expect(newCap.executable).toBeTruthy();
      expect(newCap.readable).toBeTruthy();
      expect(newCap.writable).toBeTruthy();
    });

    it('allows to change readability', () => {
      const c = new Capability({ writable: true, readable: true, executable: true });

      const newCap = c.disallowToRead().disallowToExecute().disallowToWrite();

      expect(newCap.executable).toBeFalsy();
      expect(newCap.readable).toBeFalsy();
      expect(newCap.writable).toBeFalsy();
    });
  });
});
