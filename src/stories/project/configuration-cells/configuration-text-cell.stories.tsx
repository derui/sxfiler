import { withInfo } from "@storybook/addon-info";
import { withKnobs } from "@storybook/addon-knobs";
import { storiesOf } from "@storybook/preact";
import { h } from "preact";

import { Component } from "@/components/project/configuration-cells/configuration-text-cell";
import { createItem } from "@/configurations/creators";
import { createText } from "@/configurations/item-creators";

storiesOf("Project/Configuration Cell/Text", module)
  .addParameters({ info: { inline: true } })
  .add(
    "empty value",
    () => {
      const item = createItem({
        key: ["key", "section", "item"],
        displayName: "text",
        description: "description",
        type: createText(),
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
        displayName: "text",
        description: "description",
        type: createText("test"),
      });

      return (
        <div class="theme__default">
          <Component item={item} onUpdated={() => {}} />
        </div>
      );
    },
    { decorators: [withInfo, withKnobs] }
  );
