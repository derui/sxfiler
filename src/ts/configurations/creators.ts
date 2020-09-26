import { Section, Category, Item, ItemType, ItemKey, SectionDescriptor, CategoryDescriptor } from "./types";

export const createCategory = function (
  args: Omit<CategoryDescriptor, "key"> & { key: readonly [string]; sections: Section[] }
): Category {
  const { key, ...rest } = args;
  return { key: { key: key }, ...rest };
};

export const createSection = function (
  args: Omit<SectionDescriptor, "key"> & { key: readonly [string, string]; items: Item[] }
): Section {
  const { key, ...rest } = args;
  return { key: { key: key }, ...rest };
};

export const toItemKey = function <T = any>(key: [string, string, string]): ItemKey<T> {
  return {
    type_: undefined,
    key,
  };
};

export const createItem = function (args: {
  key: ItemKey<any>;
  displayName: string;
  description: string;
  type: ItemType;
}): Item {
  const { key, ...rest } = args;
  return {
    key,
    displayName: rest.displayName,
    description: rest.description,
    type: rest.type,
  };
};
