import { pipe, compose, also } from "./index";

describe("fn library", () => {
  describe("pipe", () => {
    it("should return same result when pass only one function", () => {
      expect(pipe((v: number) => v * 2)(3)).toEqual(6);
    });

    it("should return result applied functions first to last", () => {
      const fn1 = (v: string) => `fn1(${v})`;
      const fn2 = (v: string) => `fn2(${v})`;
      const fn3 = (v: string) => `fn3(${v})`;
      expect(pipe(fn1, fn2, fn3)("inner")).toEqual("fn3(fn2(fn1(inner)))");
    });
  });

  describe("compose", () => {
    it("should return composed function", () => {
      const f = compose(
        (v) => `fn1(${v})`,
        (v) => `fn2(${v})`
      );

      expect(f(100)).toEqual("fn2(fn1(100))");
    });
  });

  describe("also", () => {
    test("can edit object in function", () => {
      const obj = { a: 0 };
      const ret = also(obj, (v) => {
        v.a = 10;
      });

      expect(obj.a).toBe(10);
      expect(obj).toBe(ret);
    });
  });
});
