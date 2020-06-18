import { Section, Category, Item, ItemType, ItemKey, SectionDescriptor, CategoryDescriptor } from "./types";

export const createCategory = function (args: CategoryDescriptor & { sections: Section[] }): Category {
  return { ...args };
};

export const createSection = function (args: SectionDescriptor & { items: Item[] }): Section {
  return { ...args };
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
