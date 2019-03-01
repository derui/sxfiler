import { withInfo } from "@storybook/addon-info";
import { boolean, withKnobs } from "@storybook/addon-knobs";
import { storiesOf } from "@storybook/react";
import * as React from "react";

import T from "../../components/project/notification-item/notification-item";
import * as N from "../../domains/notification";

storiesOf("Project/Notification Item", module)
  .addDecorator(withInfo)
  .addDecorator(withKnobs)
  .addParameters({ info: { inline: true } })
  .add("info level", () => {
    const item = N.createOneShot("id", N.Level.Info, "message");
    return <T item={item} onItemTimeouted={() => { }} timeouted={boolean("Timeouted", false)} />;
  })
  .add("Warning level", () => {
    const item = N.createOneShot("id", N.Level.Warning, "message");
    return <T item={item} onItemTimeouted={() => { }} timeouted={boolean("Timeouted", false)} />;
  })
  .add("Error level", () => {
    const item = N.createOneShot("id", N.Level.Error, "message");
    return <T item={item} onItemTimeouted={() => { }} timeouted={boolean("Timeouted", false)} />;
  })
  ;
