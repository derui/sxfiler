import { withInfo } from "@storybook/addon-info";
import { withKnobs } from "@storybook/addon-knobs";
import { storiesOf } from "@storybook/preact";
import { h } from "preact";

import { Component } from "@/components/project/configuration-cells/configuration-boolean-cell";
import { createItem, toItemKey } from "@/configurations/creators";
import { createBoolean } from "@/configurations/item-creators";

storiesOf("Project/Configuration Cell/Boolean", module)
  .addParameters({ info: { inline: true } })
  .add(
    "empty value",
    () => {
      const item = createItem({
        key: toItemKey(["key", "section", "item"]),
        displayName: "boolean",
        description: "description",
        type: createBoolean(false),
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
        key: toItemKey(["key", "section", "item"]),
        displayName: "boolean",
        description: "description",
        type: createBoolean(true),
      });

      return (
        <div class="theme__default">
          <Component item={item} onUpdated={() => {}} />
        </div>
      );
    },
    { decorators: [withInfo, withKnobs] }
  );
