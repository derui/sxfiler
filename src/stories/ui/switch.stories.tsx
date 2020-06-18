import { withInfo } from "@storybook/addon-info";
import { boolean, boolean, withKnobs } from "@storybook/addon-knobs";
import { storiesOf } from "@storybook/preact";
import { h } from "preact";

import { Component } from "@/components/ui/switch";

const style = {
  backgroundColor: "rgb(var(--color-base03))",
};

storiesOf("UI/Switch", module)
  .addParameters({ info: { inline: true } })
  .add(
    "empty value",
    () => {
      return (
        <div class="theme__default" style={style}>
          <Component onChange={() => {}} />
        </div>
      );
    },
    { decorators: [withInfo, withKnobs] }
  );
