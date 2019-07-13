import { withInfo } from "@storybook/addon-info";
import { boolean, withKnobs } from "@storybook/addon-knobs";
import { storiesOf } from "@storybook/react";
import * as React from "react";

import { Component as LogViewer } from "@/components/project/log-viewer/log-viewer";
import { createMessage } from "@/domains/message-notification";
import { Level } from "@/domains/message-notification";

const style = {
  height: "100px",
};

storiesOf("Project/Log Viewer ", module)
  .addParameters({ info: { inline: true } })
  .add(
    "empty list",
    () => {
      return (
        <div style={style}>
          <LogViewer entries={[]} hidden={boolean("hidden", false)} />
        </div>
      );
    },
    { decorators: [withInfo, withKnobs] }
  )
  .add(
    "with some entries",
    () => {
      const entries = [
        createMessage({ id: "id1", message: "info", level: Level.Info }),
        createMessage({ id: "id2", message: "error", level: Level.Error }),
        createMessage({ id: "id3", message: "warning", level: Level.Warning }),
      ];
      return (
        <div style={style}>
          <LogViewer entries={entries} hidden={boolean("hidden", false)} />
        </div>
      );
    },
    { decorators: [withInfo, withKnobs] }
  );
