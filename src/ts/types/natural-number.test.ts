import * as N from "./natural-number";

describe("Types", () => {
  describe("Natural number", () => {
    test("create natural number from 0", () => {
      const v = N.create(0);

      expect(v.value).toBe(0);
    });

    test("create natural number from value that is greater than 0", () => {
      const v = N.create(1);

      expect(v.value).toBe(1);
    });

    test("create natural number 0 when given value is less than 0", () => {
      const v = N.create(-1);

      expect(v.value).toBe(0);
    });

    test("factory function fix value as fixed number when value has exponent", () => {
      const v = N.create(1.1);

      expect(v.value).toBe(1);
    });

    test("increment natural value", () => {
      const v = N.create(0);

      expect(N.inc(v).value).toBe(1);
    });

    test("decrement natural value", () => {
      const v = N.create(2);

      expect(N.dec(v).value).toBe(1);
    });

    test("decrement natural number if it is less than 0, return 0", () => {
      const v = N.create(0);

      expect(N.dec(v).value).toBe(0);
    });

    test("add natural numbers", () => {
      const v1 = N.create(1);
      const v2 = N.create(2);

      expect(N.add(v1, v2).value).toBe(3);
    });

    test("subtract natural numbers", () => {
      const v1 = N.create(3);
      const v2 = N.create(2);

      expect(N.sub(v1, v2).value).toBe(1);
      expect(N.sub(v2, v1).value).toBe(0);
    });

    test("multiply natural numbers", () => {
      const v1 = N.create(3);
      const v2 = N.create(2);

      expect(N.mul(v1, v2).value).toBe(6);
      expect(N.mul(v1, v2)).toEqual(N.mul(v2, v1));
    });

    test("divide natural number with other number", () => {
      const v1 = N.create(4);
      const v2 = N.create(2);

      expect(N.div(v1, v2)?.value).toEqual(2);
      expect(N.div(v2, v1)?.value).toEqual(0);
    });

    test("return undefined if divide natural number by 0", () => {
      const v1 = N.create(4);
      const v2 = N.create(0);

      expect(N.div(v1, v2)).toEqual(undefined);
      expect(N.div(v2, v1)?.value).toEqual(0);
    });
  });
});
