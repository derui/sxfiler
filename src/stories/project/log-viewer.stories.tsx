import { withInfo } from "@storybook/addon-info";
import { boolean, withKnobs } from "@storybook/addon-knobs";
import { storiesOf } from "@storybook/preact";
import { h } from "preact";
import { Component as LogViewer } from "@/components/project/log-viewer";

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
      return (
        <div style={style}>
          <LogViewer entries={[]} hidden={boolean("hidden", false)} />
        </div>
      );
    },
    { decorators: [withInfo, withKnobs] }
  );
