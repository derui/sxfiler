import { withInfo } from "@storybook/addon-info";
import { number, text, withKnobs } from "@storybook/addon-knobs";
import { storiesOf } from "@storybook/react";
import * as React from "react";
import { Theme, ThemeProvider } from "@/components/theme";

import { Component as T } from "@/components/project/progress-notification-item/progress-notification-item";
import * as N from "@/domains/progress-notification";

storiesOf("Project/Progress Notification Item", module)
  .addParameters({ info: { inline: true } })
  .add(
    "progress bar",
    () => {
      const current = number("Current", 0);
      const target = number("Target", 100);
      const process = text("Process name", "process");
      const item = N.createProgress("id", {
        process,
        current,
        target,
      });
      return (
        <ThemeProvider theme={Theme}>
          <T body={item.body} />
        </ThemeProvider>
      );
    },
    { decorators: [withInfo, withKnobs] }
  );
