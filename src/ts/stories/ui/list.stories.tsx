import { withInfo } from "@storybook/addon-info";
import { storiesOf } from "@storybook/react";
import * as React from "react";

import ListItem from "../../components/ui/list-item/list-item";
import List from "../../components/ui/list/list";

// tslint:disable-next-line
const styles = require("./list.stories.module.scss");

storiesOf("UI Kit/List", module)
  .addDecorator(withInfo)
  .addParameters({ info: { inline: true } })
  .add("empty", () => {
    return <List />;
  })
  .add("with ListItem component", () => {
    return (
      <List>
        <ListItem className={styles.item}>Item 1</ListItem>
        <ListItem className={styles.item}>Item 2</ListItem>
        <ListItem className={styles.item}>Item 3</ListItem>
        <ListItem className={styles.item}>Item 4</ListItem>
      </List>
    );
  })
  .add("with style", () => {
    return (
      <List className={styles.base}>
        <ListItem>Item 1</ListItem>
      </List>
    );
  });
