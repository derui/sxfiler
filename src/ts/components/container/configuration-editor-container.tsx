import { h, FunctionalComponent } from "preact";
import { Component as SectionEditor } from "@/components/project/configuration-section";
import { Component as Navigator } from "@/components/project/configuration-navigator";
import { qualified, Configurations, findSectionBy, findValueBy } from "@/configurations";
import * as C from "@/modules";
import { Item, Section, Category } from "@/configurations/types";
import { Locator } from "@/locator";
import { descriptors } from "@/commands/internal";
import { isClosed } from "@/modules/configuration/selectors";

export type Props = {
  state: C.State;
  locator: Locator;
};

const handleSectionSelected = (locator: Locator, state: C.State) => (_: Category, section: Section) => {
  const command = locator.commandResolver?.resolveBy(descriptors.configurationSelectSection);
  if (!command) {
    return;
  }

  locator.commandExecutor?.execute(command, state, { section });
};

const handleUpdated = (locator: Locator, state: C.State) => (item: Item, value: any) => {
  const command = locator.commandResolver?.resolveBy(descriptors.configurationUpdate);
  if (!command) {
    return;
  }

  locator.commandExecutor?.execute(command, state, { itemKey: item.key, value });
};

export const Component: FunctionalComponent<Props> = ({ state, locator }) => {
  const categories = [Configurations.definitions.general];
  const itemValueMap: { [p: string]: any } = {};
  let section;
  const selectedSection = state.configuration.selectedSection;
  if (selectedSection) {
    section = findSectionBy(selectedSection, categories[0]);
    if (section) {
      section.items.reduce((accum, item) => {
        accum[qualified(item.key)] = findValueBy(state.configuration.configuration, item);
        return accum;
      }, itemValueMap);
    }
  }

  const closed = isClosed(state.configuration);

  return (
    <div class="configuration-editor" aria-hidden={closed} data-testid="configuration-editor">
      <Navigator categories={categories} onSectionSelected={handleSectionSelected(locator, state)} />
      {section && (
        <SectionEditor section={section} onUpdated={handleUpdated(locator, state)} itemValueMap={itemValueMap} />
      )}
    </div>
  );
};
