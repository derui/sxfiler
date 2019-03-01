import { withInfo } from "@storybook/addon-info";
import { array, withKnobs } from "@storybook/addon-knobs";
import { storiesOf } from "@storybook/react";
import * as React from "react";

import L from "../../components/project/notification-list/notification-list";
import * as N from "../../domains/notification";

storiesOf("Project/Notification List", module)
  .addDecorator(withInfo)
  .addDecorator(withKnobs)
  .addParameters({ info: { inline: true } })
  .add("empty", () => {
    return <L notifications={[]} onItemTimeouted={() => { }} timeouts={[]} />;
  })
  .add("with notifications", () => {
    const timeouts = array("timeouted message", []);

    const info = N.createOneShot("id", N.Level.Info, "id: message");
    const warning = N.createOneShot("id2", N.Level.Warning, "id2: warning");
    const error = N.createOneShot("id3", N.Level.Error, "id3: error");
    return <L notifications={[info, warning, error]} timeouts={timeouts} onItemTimeouted={() => { }} />;
  })
  ;
