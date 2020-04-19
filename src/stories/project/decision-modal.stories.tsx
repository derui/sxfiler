import { withInfo } from "@storybook/addon-info";
import { boolean, withKnobs } from "@storybook/addon-knobs";
import { storiesOf } from "@storybook/preact";
import { h } from "preact";
import { Component as DecisionModal } from "@/components/project/decision-modal";
import { DecisionAction } from "@/modules/decision/reducer";

storiesOf("Project/Suggestion Modal", module)
  .addParameters({ info: { inline: true } })
  .add(
    "overwrite suggestion",
    () => {
      const root = document.getElementById("modal-root");
      if (!root) {
        return <span />;
      }

      return (
        <DecisionModal
          dialogRoot={root}
          opened={boolean("opened", false)}
          container={{
            onNewNameChange: () => {},
            focusedActionKind: DecisionAction.Overwrite,
            selectableActions: [{ kind: DecisionAction.Overwrite }],
          }}
        />
      );
    },
    { decorators: [withInfo, withKnobs] }
  )
  .add(
    "rename suggention",
    () => {
      const root = document.getElementById("modal-root");
      if (!root) {
        return <span />;
      }

      return (
        <DecisionModal
          dialogRoot={root}
          opened={boolean("opened", false)}
          container={{
            onNewNameChange: () => {},
            focusedActionKind: DecisionAction.Rename,
            selectableActions: [{ kind: DecisionAction.Rename, newName: "empty.js" }],
          }}
        />
      );
    },
    { decorators: [withInfo, withKnobs] }
  )
  .add(
    "Multiple suggestions",
    () => {
      const root = document.getElementById("modal-root");
      if (!root) {
        return <span />;
      }

      return (
        <DecisionModal
          dialogRoot={root}
          opened={boolean("opened", false)}
          container={{
            onNewNameChange: () => {},
            selectableActions: [
              { kind: DecisionAction.Overwrite },
              { kind: DecisionAction.Rename, newName: "empty.js" },
            ],
          }}
        />
      );
    },
    { decorators: [withInfo, withKnobs] }
  );
