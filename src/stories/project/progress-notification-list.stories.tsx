import { withInfo } from "@storybook/addon-info";
import { number, text, withKnobs } from "@storybook/addon-knobs";
import { storiesOf } from "@storybook/react";
import * as React from "react";
import { Theme, ThemeProvider } from "@/components/theme";

import { Component as L } from "@/components/project/progress-notification-list/progress-notification-list";
import * as N from "@/domains/progress-notification";

storiesOf("Project/Progress Notification List", module)
  .addParameters({ info: { inline: true } })
  .add("empty", () => {
    return (
      <ThemeProvider theme={Theme}>
        <L notifications={[]} />
      </ThemeProvider>
    );
  })
  .add(
    "with notifications",
    () => {
      const current = number("Current", 0);
      const targeted = number("Target", 100);
      const process = text("Process name", "process");
      const item = N.createProgress("id", {
        process,
        current,
        targeted,
      });
      const item2 = N.createProgress("id", {
        process: "fixed process",
        current: 10,
        target: 87,
      });

      return (
        <ThemeProvider theme={Theme}>
          <L notifications={[item, item2]} />
        </ThemeProvider>
      );
    },
    { decorators: [withInfo, withKnobs] }
  );
