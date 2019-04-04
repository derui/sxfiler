import { withInfo } from "@storybook/addon-info";
import { boolean, withKnobs } from "@storybook/addon-knobs";
import { storiesOf } from "@storybook/react";
import * as React from "react";

import { Component as T } from "../../components/project/notification-item/notification-item";
import * as N from "../../domains/notification";

storiesOf("Project/Notification Item", module)
  .addParameters({ info: { inline: true } })
  .add(
    "info level",
    () => {
      const item = N.createMessage("id", N.Level.Info, "message");
      const onExited = () => {
        return;
      };
      return <T body={item.body} level={item.level} onExited={onExited} timeouted={boolean("Timeouted", false)} />;
    },
    { decorators: [withInfo, withKnobs] }
  )
  .add(
    "Warning level",
    () => {
      const item = N.createMessage("id", N.Level.Warning, "message");
      const onExited = () => {
        return;
      };
      return <T body={item.body} level={item.level} onExited={onExited} timeouted={boolean("Timeouted", false)} />;
    },
    { decorators: [withInfo, withKnobs] }
  )
  .add(
    "Error level",
    () => {
      const item = N.createMessage("id", N.Level.Error, "message");
      const onExited = () => {
        return;
      };
      return <T body={item.body} level={item.level} onExited={onExited} timeouted={boolean("Timeouted", false)} />;
    },
    { decorators: [withInfo, withKnobs] }
  );
