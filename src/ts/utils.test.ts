import { unwrap } from "./utils";

describe("Global utility", () => {
  describe("unwrap", () => {
    it("throw error when pass undefined", () => {
      expect(() => unwrap(undefined)).toThrowError();
    });

    it("throw error when pass null", () => {
      expect(() => unwrap(null)).toThrowError();
    });

    it("return value when the value is not undefined or null", () => {
      expect(unwrap(100)).toEqual(100);
    });
  });
});
