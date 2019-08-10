import { withInfo } from "@storybook/addon-info";
import { number, withKnobs, boolean } from "@storybook/addon-knobs";
import { storiesOf } from "@storybook/react";
import * as React from "react";
import { Theme, ThemeProvider } from "@/components/theme";

import { Component as Completer } from "@/components/project/completer/completer";
import { createCandidate } from "@/domains/candidate";

const style = {
  height: "100px",
};

storiesOf("Project/Completer", module)
  .addParameters({ info: { inline: true } })
  .add(
    "empty completion",
    () => {
      const root = document.getElementById("modal-root");
      if (!root) {
        return <span />;
      }
      return (
        <ThemeProvider theme={Theme}>
          <div style={style}>
            <Completer
              dialogRoot={root}
              opened={boolean("opened", false)}
              container={{
                title: "completion",
                items: [],
                selectedItemIndex: number("item", 0),
                onInput: () => {},
              }}
              overlay={{}}
            />
          </div>
        </ThemeProvider>
      );
    },
    { decorators: [withInfo, withKnobs] }
  )
  .add(
    "with some items",
    () => {
      const root = document.getElementById("modal-root");
      if (!root) {
        return <span />;
      }
      const items = [
        createCandidate({ id: "id1", value: "foo", start: 0, length: 0 }),
        createCandidate({ id: "id2", value: "foobar", start: 3, length: 3 }),
        createCandidate({ id: "id3", value: "barbaz", start: 0, length: 3 }),
      ];

      return (
        <ThemeProvider theme={Theme}>
          <div style={style}>
            <Completer
              dialogRoot={root}
              opened={true}
              container={{
                title: "completion",
                items,
                selectedItemIndex: number("item", 0),
                onInput: () => {},
              }}
              overlay={{}}
            />
          </div>
        </ThemeProvider>
      );
    },
    { decorators: [withInfo, withKnobs] }
  );
