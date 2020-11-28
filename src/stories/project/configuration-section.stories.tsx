import { withInfo } from "@storybook/addon-info";
import { withKnobs } from "@storybook/addon-knobs";
import { storiesOf } from "@storybook/preact";
import { h } from "preact";

import { Component } from "@/components/project/configuration-section";
import { createSection, createItem, toItemKey } from "@/configurations/creators";
import { createText, createNumber, createBoolean } from "@/configurations/item-creators";

storiesOf("Project/Configuration Section", module)
  .addParameters({ info: { inline: true } })
  .add(
    "empty section",
    () => {
      const section = createSection({
        key: ["category", "section1"],
        displayName: "section1",
        description: "description",
        items: [],
      });
      return (
        <div class="theme__default">
          <Component section={section} onUpdated={() => {}} itemValueMap={{}} />
        </div>
      );
    },
    { decorators: [withInfo, withKnobs] }
  )
  .add(
    "single item",
    () => {
      const section = createSection({
        key: ["category", "section1"],
        displayName: "section1",
        description: "looooooooooooooooooooooooooooooooong looooooooooooooooooooong long long long \ndescription",
        items: [
          createItem({
            key: toItemKey(["key", "section1", "item"]),
            displayName: "Item for Text",
            description: "input text to this item",
            type: createText(),
          }),
        ],
      });
      return (
        <div class="theme__default">
          <Component section={section} onUpdated={() => {}} itemValueMap={{}} />
        </div>
      );
    },
    { decorators: [withInfo, withKnobs] }
  )
  .add(
    "multiple items",
    () => {
      const section = createSection({
        key: ["category", "section1"],
        displayName: "section1",
        description: "description",
        items: [
          createItem({
            key: toItemKey(["key", "section1", "foo"]),
            displayName: "Item for Text",
            description: "input text to this item",
            type: createText(),
          }),
          createItem({
            key: toItemKey(["key", "section1", "bar"]),
            displayName: "Item for Number",
            description: "input number to this item",
            type: createNumber(),
          }),
          createItem({
            key: toItemKey(["key", "section1", "baz"]),
            displayName: "Item for Boolean",
            description: "toggle switch",
            type: createBoolean(false),
          }),
        ],
      });
      return (
        <div class="theme__default">
          <Component section={section} onUpdated={() => {}} itemValueMap={{}} />
        </div>
      );
    },
    { decorators: [withInfo, withKnobs] }
  );
