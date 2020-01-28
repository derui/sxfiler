import { range, between } from "./utils";

describe("Global utility", () => {
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

  describe("between", () => {
    it("should return 0 if value less than 0", () => {
      expect(between(-1, 0)).toEqual(0);
    });

    it("should return value between minimum and maximum", () => {
      expect(between(9, 8)).toEqual(8);
    });

    it("should return same value if contains between minimum and maximum", () => {
      expect(between(5, 8)).toEqual(5);
    });
  });
});
