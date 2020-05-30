import { ItemElementType, ItemType, SelectOption } from "./types";

export const createText = (value?: string): ItemType => ({ kind: ItemElementType.TEXT, defaultValue: value });

export const createNumber = (value?: number): ItemType => ({ kind: ItemElementType.NUMBER, defaultValue: value });

export const createBoolean = (value: boolean): ItemType => ({ kind: ItemElementType.BOOLEAN, defaultValue: value });

export const createSelectOne = (options: SelectOption[], value: string): ItemType => ({
  kind: ItemElementType.SELECT_ONE,
  options,
  defaultValue: value,
});
