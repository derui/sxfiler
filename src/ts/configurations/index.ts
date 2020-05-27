import { ObjectEnum } from "@/utils";
import { SortType, SortTypeMap } from "@/generated/types_pb";

export type Descriptor = {
  readonly key: string;
  readonly displayName: string;
  readonly description: string;
};

export type SelectOption = {
  value: any;
  toString(): string;
  display: string;
};

export const ItemType = {
  text: "text",
  number: "number",
  boolean: "boolean",
  selectOne: "selectOne",
} as const;
export type ItemType = ObjectEnum<typeof ItemType>;

export type ItemKind<K extends ItemType> = {
  kind: K;
};

export type TextItemType = ItemKind<"text"> & {
  defaultValue?: string;
};
export type NumberItemType = ItemKind<"number"> & {
  defaultValue?: number;
};
export type BooleanItemType = ItemKind<"boolean"> & {
  defaultValue: boolean;
};
export type SelectOneItemType = ItemKind<"selectOne"> & {
  options: SelectOption[];
  defaultValue: string;
};

export type ItemValue = TextItemType | NumberItemType | BooleanItemType | SelectOneItemType;

namespace ItemDefinitionCreators {
  export const text = (value?: string) => ({ kind: ItemType.text, defaultValue: value });
  export const number = (value?: number) => ({ kind: ItemType.number, defaultValue: value });
  export const boolean = (value: boolean) => ({ kind: ItemType.boolean, defaultValue: value });
  export const selectOne = (options: SelectOption[], value: string) => ({
    kind: ItemType.selectOne,
    options,
    defaultValue: value,
  });
}

export type Item = Descriptor & {
  readonly definition: ItemValue;
};

export type Section = Descriptor & {
  readonly items: { [p: string]: Item };
};

export type Category = Descriptor & {
  readonly sections: { [p: string]: Section };
};

const category = function (descriptor: Descriptor, sections: { [p: string]: Section }): Category {
  return { ...descriptor, sections };
};

const section = function (descriptor: Descriptor, items: { [p: string]: Item }): Section {
  return { ...descriptor, items };
};

const item = function (descriptor: Descriptor, definition: ItemValue): Item {
  return { ...descriptor, definition };
};

export const findBy = (key: string[], category: Category): Item | undefined => {
  const [categoryKey, sectionKey, itemKey] = key;
  if (category.key !== categoryKey) {
    return;
  }
  const section = category.sections[sectionKey];
  if (!section) {
    return;
  }

  return section.items[itemKey];
};

export type Key<T> = readonly [string, string, string];
const asText = (key: [string, string, string]): Key<string> => key;
const asNumber = (key: [string, string, string]): Key<number> => key;
const asBoolean = (key: [string, string, string]): Key<boolean> => key;
const asType = <T>(key: [string, string, string]): Key<T> => key;

export const definition = {
  general: {
    behaviors: {
      confirmationWhenDelete: asBoolean(["general", "behaviors", "confirmation_when_delete"]),
      maxNumberOfHistories: asNumber(["general", "behaviors", "max_number_of_histories"]),
      defaultSortOrder: asType<SortTypeMap>(["general", "behaviors", "default_sort_order"]),
    },
    theme: {
      currentTheme: asText(["general", "theme", "current_theme"]),
    },
  },
} as const;

export namespace Configurations {
  const general = category(
    { key: "general", displayName: "General", description: "General configurations" },
    {
      behaviors: section(
        { key: "behaviors", displayName: "Behaviors", description: "Behaviors" },
        {
          confirmationWhenDelete: item(
            {
              key: "confirmation_when_delete",
              displayName: "Confirmation when delete",
              description: "Need user confirmation when delete something",
            },
            ItemDefinitionCreators.boolean(true)
          ),
          maxNumberOfHistories: item(
            {
              key: "max_number_of_histories",
              displayName: "Maximum number of histories",
              description:
                "History is the history of moved directory in a pane. This setting defines maximum number of histories in a pane",
            },
            ItemDefinitionCreators.number(100)
          ),
          defaultSortOrder: item(
            {
              key: "default_sort_order",
              displayName: "Default sort order of Pane",
              description: "This setting defines sort order of filer when open this application",
            },
            ItemDefinitionCreators.selectOne(
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
            )
          ),
        }
      ),
      theme: section(
        { key: "theme", displayName: "Theme", description: "Theme" },
        {
          currentTheme: item(
            { key: "current_theme", displayName: "Current theme", description: "Select current theme" },
            ItemDefinitionCreators.text("default")
          ),
        }
      ),
    }
  );

  export const definitions = {
    general,
  } as const;
}
