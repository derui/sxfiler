import { withInfo } from "@storybook/addon-info";
import { boolean, withKnobs } from "@storybook/addon-knobs";
import { storiesOf } from "@storybook/react";
import * as React from "react";

import * as Element from "@/components/ui/element/element";
import * as ListItem from "@/components/ui/list-item/list-item";

// eslint-disable-next-line @typescript-eslint/no-var-requires
const styles = require("./list-item.stories.module.scss");

storiesOf("UI Kit/List Item", module)
  .addParameters({ info: { inline: true } })
  .add(
    "with text",
    () => {
      return <ListItem.Component selected={boolean("Selected", false)}> Text</ListItem.Component>;
    },
    { decorators: [withInfo, withKnobs] }
  )
  .add(
    "with other component",
    () => {
      return (
        <ListItem.Component selected={boolean("Selected", false)}>
          <span style={{ color: "red" }}>Text in span</span>
        </ListItem.Component>
      );
    },
    { decorators: [withInfo, withKnobs] }
  )
  .add(
    "with className",
    () => {
      return (
        <ListItem.Component
          selected={boolean("Selected", false)}
          className={`${styles.base} ${styles.padding} ${styles.border}`}
        >
          Item
        </ListItem.Component>
      );
    },
    { decorators: [withInfo, withKnobs] }
  )
  .add(
    "with other tagName",
    () => {
      const C = ListItem.createComponent({ container: Element.createComponent({ tagName: "a" }) });
      return <C selected={boolean("Selected", false)}>Link is container</C>;
    },
    { decorators: [withInfo, withKnobs] }
  );
