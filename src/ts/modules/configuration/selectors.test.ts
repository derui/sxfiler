import * as S from "./selectors";

describe("Modules", () => {
  describe("Configuration", () => {
    describe("Selectors", () => {
      test("get all keys", () => {
        const result = S.selectAllKeys({
          configuration: {
            "key.test": 1,
            "key.second": 3,
          },
        });
        expect(result.sort()).toEqual(["key.test", "key.second"].sort());
      });
    });
  });
});
