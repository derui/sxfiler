import { h } from "preact";
import { Item, ItemElementType } from "@/configurations/types";
import { Component as TextCell } from "./configuration-cells/configuration-text-cell";
import { Component as NumberCell } from "./configuration-cells/configuration-number-cell";
import { Component as BooleanCell } from "./configuration-cells/configuration-boolean-cell";
import { Component as SelectOneCell } from "./configuration-cells/configuration-select-one-cell";

export type Props = {
  item: Item;
  value?: any;
  onUpdated: (value: any, item: Item) => void;
};

export const Component: preact.FunctionComponent<Props> = ({ item, onUpdated, value }) => {
  switch (item.type.kind) {
    case ItemElementType.TEXT:
      return <TextCell item={item} value={value} onUpdated={(v) => onUpdated(v, item)} />;
    case ItemElementType.NUMBER:
      return <NumberCell item={item} value={value} onUpdated={(v) => onUpdated(v, item)} />;
    case ItemElementType.BOOLEAN:
      return <BooleanCell item={item} value={value} onUpdated={(v) => onUpdated(v, item)} />;
    case ItemElementType.SELECT_ONE:
      return <SelectOneCell item={item} onUpdated={(v) => onUpdated(v, item)} />;
  }
};
