import { withInfo } from "@storybook/addon-info";
import { boolean, number, withKnobs } from "@storybook/addon-knobs";
import { storiesOf } from "@storybook/react";
import * as React from "react";

import { Component as SuggestionModal } from "../../components/project/suggestion-modal/suggestion-modal";
import { createOverwritePayload, createRenamePayload } from "../../domains/task-reply";

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
        <SuggestionModal
          dialogRoot={root}
          opened={boolean("opened", false)}
          overlay={{}}
          container={{
            onReply: () => {},
            focusedReply: 0,
            replies: [createOverwritePayload()],
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
        <SuggestionModal
          dialogRoot={root}
          opened={boolean("opened", false)}
          overlay={{}}
          container={{
            onReply: () => {},
            focusedReply: 0,
            replies: [createRenamePayload("node")],
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
      const replies = [createOverwritePayload(), createRenamePayload("node")];

      return (
        <SuggestionModal
          dialogRoot={root}
          opened={boolean("opened", false)}
          overlay={{}}
          container={{
            onReply: () => {},
            focusedReply: number("selected", 0),
            replies,
          }}
        />
      );
    },
    { decorators: [withInfo, withKnobs] }
  );
