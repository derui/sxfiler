import {
  createCapability,
  emptyCapability,
  fullCapability,
  allowToWrite,
  allowToRead,
  allowToExecute,
  disallowToWrite,
  disallowToRead,
  disallowToExecute,
} from "./capability";

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

  describe("change capability", () => {
    const empty = emptyCapability();
    const full = fullCapability();

    it("should be able to allow to write", () => {
      expect(allowToWrite(empty).writable).toBeTruthy();
    });

    it("should be able to allow to read", () => {
      expect(allowToRead(empty).readable).toBeTruthy();
    });

    it("should be able to allow to execute", () => {
      expect(allowToExecute(empty).executable).toBeTruthy();
    });

    it("should be able to disallow to write", () => {
      expect(disallowToWrite(full).writable).toBeFalsy();
    });

    it("should be able to change readable", () => {
      expect(disallowToRead(full).readable).toBeFalsy();
    });

    it("should be able to change executable", () => {
      expect(disallowToExecute(full).executable).toBeFalsy();
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
