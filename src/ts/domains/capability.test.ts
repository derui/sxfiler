import { createCapability, emptyCapability, fullCapability } from "./capability";

describe("Capability", () => {
  describe("createCapability", () => {
    it("create with capable actions", () => {
      const v = createCapability({
        executable: false,
        readable: true,
        writable: false,
      });

      expect(v.executable).toBeFalsy();
      expect(v.readable).toBeTruthy();
      expect(v.writable).toBeFalsy();
    });
  });

  describe("emptyCapability", () => {
    it("get instance that have no any capability", () => {
      const v = emptyCapability();

      expect(v.executable).toBeFalsy();
      expect(v.readable).toBeFalsy();
      expect(v.writable).toBeFalsy();
    });
  });

  describe("fullCapability", () => {
    it("get instance that have all capability", () => {
      const v = fullCapability();

      expect(v.executable).toBeTruthy();
      expect(v.readable).toBeTruthy();
      expect(v.writable).toBeTruthy();
    });
  });
});
