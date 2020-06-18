// this component shows category tree with configuration structure.

import { h, FunctionalComponent } from "preact";
import classnames from "classnames";
import { Category, Section, CategoryKey } from "@/configurations/types";
import { useState, StateUpdater } from "preact/hooks";
import { qualified } from "@/configurations";

export type Props = {
  categories: Category[];
  onSectionSelected: (category: Category, section: Section) => void;
};

type CategoryTreeProps = {
  category: Category;
  opened: boolean;
  onToggle: (key: CategoryKey) => void;
  onSelected: (category: Category, section: Section) => void;
};

type SectionProps = {
  section: Section;
  selected: boolean;
  onSelected: (section: Section) => void;
};

const Section: FunctionalComponent<SectionProps> = ({ section, selected, onSelected }) => {
  const className = classnames({
    "configuration-navigator__section": true,
    "configuration-navigator__section--selected": selected,
  });
  return (
    <li class={className} data-testid="configuration-navigator-section" onClick={() => onSelected(section)}>
      {section.displayName}
    </li>
  );
};

const CategoryTree: FunctionalComponent<CategoryTreeProps> = ({ category, opened, onToggle, onSelected }) => {
  const key = category.key;
  const className = `
    configuration-navigator__category-name
    ${opened ? "configuration-navigator__category-name--opened" : ""}
  `;

  return (
    <div class="configuration-navigator__category" data-testid="configuration-navigator-category">
      <span class={className} data-testid="configuration-navigator-category-name" onClick={() => onToggle(key)}>
        {category.displayName}
      </span>
      <ul
        class="configuration-navigator__section-tree"
        data-testid="configuration-navigator-section-tree"
        aria-hidden={!opened}
      >
        {Object.entries(category.sections).map(([, value], index) => (
          <Section
            key={index}
            section={value}
            selected={false}
            onSelected={(section) => onSelected(category, section)}
          />
        ))}
      </ul>
    </div>
  );
};

const handleCategoryClick = (setState: StateUpdater<{ [p: string]: boolean }>) => (categoryKey: CategoryKey) => {
  setState((prev) => {
    const current = prev[qualified(categoryKey)] || false;
    return { ...prev, [qualified(categoryKey)]: !current };
  });
};

export const Component: FunctionalComponent<Props> = ({ categories, onSectionSelected }) => {
  const [state, setState] = useState({} as { [p: string]: boolean });

  return (
    <section class="configuration-navigator">
      {categories.map((v, index) => {
        const opened = state[qualified(v.key)] || false;
        return (
          <CategoryTree
            key={index}
            category={v}
            opened={opened}
            onSelected={onSectionSelected}
            onToggle={handleCategoryClick(setState)}
          />
        );
      })}
    </section>
  );
};
