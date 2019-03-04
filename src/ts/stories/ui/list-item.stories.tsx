import { withInfo } from "@storybook/addon-info";
import { boolean, withKnobs } from "@storybook/addon-knobs";
import { storiesOf } from "@storybook/react";
import * as React from "react";

import * as Element from "../../components/ui/element/element";
import * as ListItem from "../../components/ui/list-item/list-item";

// tslint:disable-next-line
const styles = require("./list-item.stories.module.scss");

storiesOf("UI Kit/List Item", module)
  .addDecorator(withInfo)
  .addDecorator(withKnobs)
  .addParameters({ info: { inline: true } })
  .add("with text", () => {
    const C = ListItem.createComponent();
    return <ListItem.Component selected={boolean("Selected", false)}> Text</ListItem.Component>;
  })
  .add("with other component", () => {
    return (
      <ListItem.Component selected={boolean("Selected", false)}>
        <span style={{ color: "red" }}>Text in span</span>
      </ListItem.Component>
    );
  })
  .add("with className", () => {
    return (
      <ListItem.Component
        selected={boolean("Selected", false)}
        className={`${styles.base} ${styles.padding} ${styles.border}`}
      >
        Item
      </ListItem.Component>
    );
  })
  .add("with other tagName", () => {
    const C = ListItem.createComponent({ container: Element.createComponent({ tagName: "a" }) });
    return <C selected={boolean("Selected", false)}>Link is container</C>;
  });
