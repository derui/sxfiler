import { withInfo } from "@storybook/addon-info";
import { array, withKnobs } from "@storybook/addon-knobs";
import { storiesOf } from "@storybook/react";
import * as React from "react";

import { Component as L } from "../../components/project/notification-list/notification-list";
import * as N from "../../domains/notification";

storiesOf("Project/Notification List", module)
  .addDecorator(withInfo)
  .addDecorator(withKnobs)
  .addParameters({ info: { inline: true } })
  .add("empty", () => {
    const handle = () => {
      return;
    };
    return <L notifications={[]} onNotificationHidden={handle} timeouts={[]} />;
  })
  .add("with notifications", () => {
    const handle = () => {
      return;
    };
    const timeouts = array("timeouted message", []);

    const info = N.createMessage("id", N.Level.Info, "id: message");
    const warning = N.createMessage("id2", N.Level.Warning, "id2: warning");
    const error = N.createMessage("id3", N.Level.Error, "id3: error");
    return <L notifications={[info, warning, error]} timeouts={timeouts} onNotificationHidden={handle} />;
  });
