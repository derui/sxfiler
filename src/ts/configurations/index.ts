import { SortType, SortTypeMap } from "@/generated/types_pb";
import { Category, Item, Section, ItemKey, SectionKey, CategoryKey } from "./types";
import { createCategory, createSection, createItem } from "./creators";
import * as ItemCreators from "./item-creators";

const itemKeyFrom = <T>(section: SectionKey, key: string): ItemKey<T> => [section[0], section[1], key];
const categoryKeyOf = (key: ItemKey<any>): CategoryKey => [key[0]];
const sectionKeyOf = (key: ItemKey<any>): SectionKey => [key[0], key[1]];
const sectionKeyFrom = (category: CategoryKey, key: string): SectionKey => [category[0], key];
export const qualified = (key: readonly string[]): string => key.join(".");

export const findItemBy = (key: ItemKey<any>, category: Category): Item | undefined => {
  const categoryKey = categoryKeyOf(key);
  const sectionKey = sectionKeyOf(key);
  if (qualified(category.key) !== qualified(categoryKey)) {
    return;
  }
  const section = category.sections.find((v) => v.key === sectionKey);
  if (!section) {
    return;
  }

  return section.items.find((v) => qualified(v.key) === qualified(key));
};

export const findSectionBy = (key: SectionKey, category: Category): Section | undefined => {
  return category.sections.find((v) => qualified(v.key) === qualified(key));
};

export const findValueBy = (configuration: { [p: string]: any }, item: Item): any => {
  const value = configuration[qualified(item.key)];
  if (value === undefined || value === null) {
    return;
  }

  return value;
};

export const categories = {
  general: ["general"],
} as const;

export const sections = {
  behaviors: sectionKeyFrom(categories.general, "behaviors"),
  theme: sectionKeyFrom(categories.general, "theme"),
} as const;

export const itemKeys = {
  general: {
    behaviors: {
      confirmationWhenDelete: itemKeyFrom<string>(sections.behaviors, "confirmation_when_delete"),
      maxNumberOfHistories: itemKeyFrom<number>(sections.behaviors, "max_number_of_histories"),
      defaultSortOrder: itemKeyFrom<SortTypeMap>(sections.behaviors, "default_sort_order"),
    },
    theme: {
      currentTheme: itemKeyFrom<string>(sections.theme, "current_theme"),
    },
  },
} as const;

export namespace Configurations {
  const general = createCategory({
    key: categories.general,
    displayName: "General",
    description: "General configurations",
    sections: [
      createSection({
        key: sections.behaviors,
        displayName: "Behaviors",
        description: "Behaviors",
        items: [
          createItem({
            key: itemKeys.general.behaviors.confirmationWhenDelete,
            displayName: "Confirmation when delete",
            description: "Need user confirmation when delete something",
            type: ItemCreators.createBoolean(true),
          }),
          createItem({
            key: itemKeys.general.behaviors.maxNumberOfHistories,
            displayName: "Maximum number of histories",
            description:
              "History is the history of moved directory in a pane. This setting defines maximum number of histories in a pane",
            type: ItemCreators.createNumber(100),
          }),
          createItem({
            key: itemKeys.general.behaviors.defaultSortOrder,
            displayName: "Default sort order of Pane",
            description: "This setting defines sort order of filer when open this application",
            type: ItemCreators.createSelectOne(
              [
                {
                  display: "Date",
                  toString() {
                    return `${SortType.DATE}`;
                  },
                  value: SortType.DATE,
                },
                {
                  display: "Name",
                  toString() {
                    return `${SortType.NAME}`;
                  },
                  value: SortType.NAME,
                },
                {
                  display: "Size",
                  toString() {
                    return `${SortType.SIZE}`;
                  },
                  value: SortType.SIZE,
                },
              ],
              `${SortType.NAME}`
            ),
          }),
        ],
      }),
      createSection({
        key: sections.theme,
        displayName: "Theme",
        description: "Theme",
        items: [
          createItem({
            key: itemKeys.general.theme.currentTheme,
            displayName: "Current theme",
            description: "Select current theme",
            type: ItemCreators.createText("default"),
          }),
        ],
      }),
    ],
  });

  export const definitions = {
    general,
  } as const;
}
