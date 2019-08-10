import { withInfo } from "@storybook/addon-info";
import { storiesOf } from "@storybook/react";
import * as React from "react";

import { styled, Theme, ThemeProvider } from "@/components/theme";

import * as ListItem from "@/components/ui/list-item/list-item";
import * as List from "@/components/ui/list/list";

const Base = styled(List.Component)`
  ${List.style}
  border: 1px solid;
  padding: 1em;
`;

const Item = styled(ListItem.Component)`
  ${ListItem.style}
  padding: 8px;
  background-color: green;
`;

storiesOf("UI Kit/List", module)
  .addDecorator(withInfo)
  .addParameters({ info: { inline: true } })
  .add("empty", () => {
    return (
      <ThemeProvider theme={Theme}>
        <List.Component />
      </ThemeProvider>
    );
  })
  .add("with ListItem component", () => {
    return (
      <ThemeProvider theme={Theme}>
        <Base>
          <Item>Item 1</Item>
          <Item>Item 2</Item>
          <Item>Item 3</Item>
          <Item>Item 4</Item>
        </Base>
      </ThemeProvider>
    );
  })
  .add("with style", () => {
    return (
      <ThemeProvider theme={Theme}>
        <Base>
          <Item>Item 1</Item>
        </Base>
      </ThemeProvider>
    );
  });
