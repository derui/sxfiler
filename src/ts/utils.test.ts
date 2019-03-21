import { unwrap, range } from "./utils";

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

  describe("range", () => {
    it("get empty iterator when start = end", () => {
      const array = Array.from(range(0, 0));

      expect(array).toHaveLength(0);
    });

    it("get empty iterator when start > end", () => {
      const array = Array.from(range(1, 0));

      expect(array).toHaveLength(0);
    });

    it("get valid ranget when start < end", () => {
      const array = Array.from(range(1, 10));

      expect(array).toEqual([1, 2, 3, 4, 5, 6, 7, 8, 9]);
    });
  });
});
