import * as S from "./selectors";
import { DisplayState } from "./reducer";

describe("Modules", () => {
  describe("Configuration", () => {
    describe("Selectors", () => {
      test("get all keys", () => {
        const result = S.selectAllKeys({
          configuration: {
            "key.test": 1,
            "key.second": 3,
          },
          displayState: DisplayState.Closed,
          selectedSection: null,
        });
        expect(result.sort()).toEqual(["key.test", "key.second"].sort());
      });
    });
  });
});
