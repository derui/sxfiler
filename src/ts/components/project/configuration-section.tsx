import { h, FunctionalComponent } from "preact";
import { Section, Item } from "@/configurations/types";
import { Component as Cell } from "./configuration-cell";
import { qualified } from "@/configurations";

type Callback = (item: Item, value: any) => void;

export type Props = {
  onUpdated: Callback;
  section: Section;
  itemValueMap: { [p: string]: any };
};

const handleItemUpdated = (onUpdated: Callback) => (value: any, item: Item) => {
  onUpdated(item, value);
};

const makeItem = (key: string, item: Item, value: any, onUpdated: Callback) => {
  return <Cell key={key} item={item} value={value} onUpdated={handleItemUpdated(onUpdated)} />;
};

export const Component: FunctionalComponent<Props> = ({ onUpdated, section, itemValueMap }) => {
  const items = section.items;
  return (
    <div class="configuration-section" data-testid="configuration-section">
      <header class="configuration-section__header" data-testid="configuration-section-header">
        {section.description}
      </header>
      <section class="configuration-section__cell-container">
        {Object.entries(items).map(([key, value]) =>
          makeItem(key, value, itemValueMap[qualified(value.key)], onUpdated)
        )}
      </section>
    </div>
  );
};
