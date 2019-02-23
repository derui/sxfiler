import { withInfo } from "@storybook/addon-info";
import { storiesOf } from "@storybook/react";
import * as React from "react";

import ListItem from "../components/ui/list-item/list-item";
// tslint:disable-next-line
const styles = require('./list-item.stories.module.scss');

storiesOf("List Item", module)
  .addDecorator(withInfo)
  .addParameters({ info: { inline: true } })
  .add("with text", () => {
    return <ListItem>Text</ListItem>;
  })
  .add("with other component", () => {
    return (
      <ListItem>
        <span style={{ color: "red" }}>Text in span</span>
      </ListItem>
    );
  })
  .add("with class names", () => {
    return <ListItem classes={[styles.base, styles.padding, styles.border]}>Item</ListItem>;
  })
  .add("with other container", () => {
    return <ListItem container="a">Link is container</ListItem>;
  });
