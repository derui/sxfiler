import { withInfo } from "@storybook/addon-info";
import { storiesOf } from "@storybook/react";
import * as React from "react";

import * as ListItem from "../../components/ui/list-item/list-item";
import * as List from "../../components/ui/list/list";

// eslint-disable-next-line @typescript-eslint/no-var-requires
const styles = require("./list.stories.module.scss");

storiesOf("UI Kit/List", module)
  .addDecorator(withInfo)
  .addParameters({ info: { inline: true } })
  .add("empty", () => {
    return <List.Component />;
  })
  .add("with ListItem component", () => {
    return (
      <List.Component>
        <ListItem.Component className={styles.item}>Item 1</ListItem.Component>
        <ListItem.Component className={styles.item}>Item 2</ListItem.Component>
        <ListItem.Component className={styles.item}>Item 3</ListItem.Component>
        <ListItem.Component className={styles.item}>Item 4</ListItem.Component>
      </List.Component>
    );
  })
  .add("with style", () => {
    return (
      <List.Component className={styles.base}>
        <ListItem.Component>Item 1</ListItem.Component>
      </List.Component>
    );
  });
