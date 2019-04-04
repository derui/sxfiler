import { withInfo } from "@storybook/addon-info";
import { number, text, withKnobs } from "@storybook/addon-knobs";
import { storiesOf } from "@storybook/react";
import * as React from "react";

import { Component as T } from "../../components/project/progress-notification-item/progress-notification-item";
import * as N from "../../domains/notification";

storiesOf("Project/Progress Notification Item", module)
  .addParameters({ info: { inline: true } })
  .add(
    "progress bar",
    () => {
      const current = number("Current", 0);
      const target = number("Target", 100);
      const process = text("Process name", "process");
      const item = N.createProgress("id", N.Level.Info, {
        process,
        current,
        target,
      });
      return <T body={item.body} />;
    },
    { decorators: [withInfo, withKnobs] }
  );
