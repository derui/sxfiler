import { capabilityOfBits } from "./capability";

describe("File Stat", () => {
  describe("Capability", () => {
    it("should be disabled all permission for initial state", () => {
      const capability = capabilityOfBits(0o0);

      expect(capability.readable).toBeFalsy();
      expect(capability.writable).toBeFalsy();
      expect(capability.executable).toBeFalsy();
    });

    it("allows to allow some permission", () => {
      const capability = capabilityOfBits(0);

      const newCap = capability
        .allowToExecute()
        .allowToRead()
        .allowToWrite();

      expect(newCap.executable).toBeTruthy();
      expect(newCap.readable).toBeTruthy();
      expect(newCap.writable).toBeTruthy();
    });

    it("allows to change readability", () => {
      const c = capabilityOfBits(0o7);

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
