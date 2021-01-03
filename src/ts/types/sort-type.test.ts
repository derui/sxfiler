import { SortType } from "@/generated/types_pb";
import * as S from "./sort-type";

describe("Types", () => {
  describe("Sort Type", () => {
    test("should return undefined if given sort type is out of range ", () => {
      const actual = S.fromSortTypeMap(-1);

      expect(actual).toBeUndefined();
    });

    test("should return enum if given sort type is in range", () => {
      const actual = S.fromSortTypeMap(SortType.NAME);

      expect(actual).toEqual(S.SortType.NAME);
    });

    test("should return enum if given sort type is in range", () => {
      const actual = S.fromSortTypeMap(SortType.DATE);

      expect(actual).toEqual(S.SortType.DATE);
    });

    test("should return enum if given sort type is in range", () => {
      const actual = S.fromSortTypeMap(SortType.SIZE);

      expect(actual).toEqual(S.SortType.SIZE);
    });
  });
});
