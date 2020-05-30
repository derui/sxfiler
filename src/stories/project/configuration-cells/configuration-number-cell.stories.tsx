import { withInfo } from "@storybook/addon-info";
import { withKnobs } from "@storybook/addon-knobs";
import { storiesOf } from "@storybook/preact";
import { h } from "preact";

import { Component } from "@/components/project/configuration-cells/configuration-number-cell";
import { createItem } from "@/configurations/creators";
import { createNumber } from "@/configurations/item-creators";

storiesOf("Project/Configuration Cell/Number", module)
  .addParameters({ info: { inline: true } })
  .add(
    "empty value",
    () => {
      const item = createItem({
        key: ["key", "section", "item"],
        displayName: "number",
        description: "description",
        type: createNumber(),
      });

      return (
        <div class="theme__default">
          <Component item={item} onUpdated={() => {}} />
        </div>
      );
    },
    { decorators: [withInfo, withKnobs] }
  )
  .add(
    "default value",
    () => {
      const item = createItem({
        key: ["key", "section", "item"],
        displayName: "number",
        description: "description",
        type: createNumber(15918),
      });

      return (
        <div class="theme__default">
          <Component item={item} onUpdated={() => {}} />
        </div>
      );
    },
    { decorators: [withInfo, withKnobs] }
  );
