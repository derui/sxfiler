import { withInfo } from "@storybook/addon-info";
import { withKnobs } from "@storybook/addon-knobs";
import { storiesOf } from "@storybook/preact";
import { h } from "preact";

import { Component } from "@/components/project/configuration-cells/configuration-select-one-cell";
import { createItem } from "@/configurations/creators";
import { createSelectOne } from "@/configurations/item-creators";

storiesOf("Project/Configuration Cell/Select One", module)
  .addParameters({ info: { inline: true } })
  .add(
    "default value",
    () => {
      const item = createItem({
        key: ["key", "section", "item"],
        displayName: "select one option",
        description: "description",
        type: createSelectOne(
          [
            {
              value: 100,
              toString() {
                return String(this.value);
              },
              display: "value: 100",
            },
            {
              value: "any",
              toString() {
                return String(this.value);
              },
              display: "value: any",
            },
          ],
          ""
        ),
      });

      return (
        <div class="theme__default">
          <Component item={item} onUpdated={() => {}} />
        </div>
      );
    },
    { decorators: [withInfo, withKnobs] }
  );
