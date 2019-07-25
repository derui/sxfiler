import { pipe, compose } from "./index";

describe("fn library", () => {
  describe("pipe", () => {
    it("should return same result when pass only one function", () => {
      expect(pipe((v: number) => v * 2)(3)).toEqual(6);
    });

    it("should return result applied functions first to last", () => {
      const fn1 = (v: string) => `fn1(${v})`;
      const fn2 = (v: string) => `fn2(${v})`;
      expect(
        pipe(
          fn1,
          fn2
        )("inner")
      ).toEqual("fn2(fn1(inner))");
    });
  });

  describe("compose", () => {
    it("should return composed function", () => {
      const f = compose(
        v => `fn1(${v})`,
        v => `fn2(${v})`
      );

      expect(f(100)).toEqual("fn2(fn1(100))");
    });
  });
});
