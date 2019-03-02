import { withInfo } from "@storybook/addon-info";
import { number, text, withKnobs } from "@storybook/addon-knobs";
import { storiesOf } from "@storybook/react";
import * as React from "react";

import { Component as L } from "../../components/project/progress-notification-list/progress-notification-list";
import * as N from "../../domains/notification";

storiesOf("Project/Progress Notification List", module)
  .addDecorator(withInfo)
  .addDecorator(withKnobs)
  .addParameters({ info: { inline: true } })
  .add("empty", () => {
    return <L notifications={[]} />;
  })
  .add("with notifications", () => {
    const current = number("Current", 0);
    const target = number("Target", 100);
    const process = text("Process name", "process");
    const item = N.createProgress("id", N.Level.Info, {
      process,
      current,
      target,
    });
    const item2 = N.createProgress("id", N.Level.Info, {
      process: "fixed process",
      current: 10,
      target: 87,
    });

    return <L notifications={[item, item2]} />;
  });
