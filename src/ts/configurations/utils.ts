import { ItemElementType, Item } from "./types";

// eslint-disable-next-line @typescript-eslint/no-unused-vars
export const isTypedText = function (arg: Item, value: any): value is string {
  return arg.type.kind === ItemElementType.TEXT && value !== undefined && value !== null;
};

// eslint-disable-next-line @typescript-eslint/no-unused-vars
export const isTypedNumber = function (arg: Item, value: any): value is number {
  return arg.type.kind === ItemElementType.NUMBER && value !== undefined && value !== null;
};

// eslint-disable-next-line @typescript-eslint/no-unused-vars
export const isTypedBoolean = function (arg: Item, value: any): value is boolean {
  return arg.type.kind === ItemElementType.BOOLEAN && value !== undefined && value !== null;
};
