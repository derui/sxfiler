import { ObjectEnum } from "@/utils";

export type ItemKey<T> = readonly [string, string, string];
export type SectionKey = readonly [string, string];
export type CategoryKey = readonly [string];

export type Descriptor<T> = {
  readonly key: T;
  readonly displayName: string;
  readonly description: string;
};

export type CategoryDescriptor = Descriptor<CategoryKey>;

export type SectionDescriptor = Descriptor<SectionKey>;

export type ItemDescriptor = Descriptor<ItemKey<any>>;

export type SelectOption = {
  value: any;
  toString(): string;
  display: string;
};

export const ItemElementType = {
  TEXT: "TEXT",
  NUMBER: "NUMBER",
  BOOLEAN: "BOOLEAN",
  SELECT_ONE: "SELECT_ONE",
} as const;
export type ItemElementType = ObjectEnum<typeof ItemElementType>;

export type ItemKind<K extends ItemElementType> = {
  kind: K;
};

export type TextItemType = ItemKind<"TEXT"> & {
  defaultValue?: string;
};
export type NumberItemType = ItemKind<"NUMBER"> & {
  defaultValue?: number;
};
export type BooleanItemType = ItemKind<"BOOLEAN"> & {
  defaultValue: boolean;
};
export type SelectOneItemType = ItemKind<"SELECT_ONE"> & {
  options: SelectOption[];
  defaultValue: string;
};

export type ItemType = TextItemType | NumberItemType | BooleanItemType | SelectOneItemType;

export type Item = ItemDescriptor & {
  readonly type: ItemType;
};

export type Section = SectionDescriptor & {
  readonly items: Item[];
};

export type Category = CategoryDescriptor & {
  readonly sections: Section[];
};
