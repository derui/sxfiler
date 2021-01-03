import { SortType as _SortType } from "@/generated/types_pb";

export enum SortType {
  NAME,
  SIZE,
  DATE,
}

/**
 * @param sortType base sort type
 * @returns enum or undefined
 */
export const fromSortTypeMap = function fromSortTypeMap(sortType: number): SortType | undefined {
  switch (sortType) {
    case _SortType.DATE:
      return SortType.DATE;
    case _SortType.NAME:
      return SortType.NAME;
    case _SortType.SIZE:
      return SortType.SIZE;
    default:
      return undefined;
  }
};
