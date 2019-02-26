import { Capability } from "./capability";

describe("File Stat", () => {
  describe("Capability", () => {
    it("should be disabled all permission for initial state", () => {
      const capability = new Capability();

      expect(capability.readable).toBeFalsy();
      expect(capability.writable).toBeFalsy();
      expect(capability.executable).toBeFalsy();
    });

    it("allows to allow some permission", () => {
      const capability = new Capability();

      const newCap = capability
        .allowToExecute()
        .allowToRead()
        .allowToWrite();

      expect(newCap.executable).toBeTruthy();
      expect(newCap.readable).toBeTruthy();
      expect(newCap.writable).toBeTruthy();
    });

    it("allows to change readability", () => {
      const c = new Capability({ writable: true, readable: true, executable: true });

      const newCap = c
        .disallowToRead()
        .disallowToExecute()
        .disallowToWrite();

      expect(newCap.executable).toBeFalsy();
      expect(newCap.readable).toBeFalsy();
      expect(newCap.writable).toBeFalsy();
    });
  });
});
