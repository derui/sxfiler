import { withInfo } from "@storybook/addon-info";
import { boolean, withKnobs } from "@storybook/addon-knobs";
import { storiesOf } from "@storybook/react";

import { styled, Theme, ThemeProvider } from "@/components/theme";
import * as React from "react";

import * as Element from "@/components/ui/element";
import * as ListItem from "@/components/ui/list-item";

const Styled = styled(ListItem.Component)`
  ${ListItem.style}

  padding: 1em;
  border: 1px solid;
  &[aria-selected="true"] {
    background-color: aqua;
  }
`;

const StyledAnotherTag = styled(ListItem.createComponent({ container: Element.createComponent({ tagName: "a" }) }))`
  ${ListItem.style}

  padding: 1em;
  border: 1px solid;
  &[aria-selected="true"] {
    background-color: aqua;
  }
`;

storiesOf("UI Kit/List Item", module)
  .addParameters({ info: { inline: true } })
  .add(
    "with text",
    () => {
      return (
        <ThemeProvider theme={Theme}>
          <Styled selected={boolean("Selected", false)}> Text</Styled>
        </ThemeProvider>
      );
    },
    { decorators: [withInfo, withKnobs] }
  )
  .add(
    "with other component",
    () => {
      return (
        <ThemeProvider theme={Theme}>
          <Styled selected={boolean("Selected", false)}>
            <span style={{ color: "red" }}>Text in span</span>
          </Styled>
        </ThemeProvider>
      );
    },
    { decorators: [withInfo, withKnobs] }
  )
  .add(
    "with className",
    () => {
      return (
        <ThemeProvider theme={Theme}>
          <Styled selected={boolean("Selected", false)}>Item</Styled>
        </ThemeProvider>
      );
    },
    { decorators: [withInfo, withKnobs] }
  )
  .add(
    "with other tagName",
    () => {
      return (
        <ThemeProvider theme={Theme}>
          <StyledAnotherTag selected={boolean("Selected", false)}>Link is container</StyledAnotherTag>
        </ThemeProvider>
      );
    },
    { decorators: [withInfo, withKnobs] }
  );
