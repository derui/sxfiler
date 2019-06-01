import { withInfo } from "@storybook/addon-info";
import { boolean, withKnobs } from "@storybook/addon-knobs";
import { storiesOf } from "@storybook/react";
import * as React from "react";

import { Component as SuggestionModal } from "../../components/project/suggestion-modal/suggestion-modal";
import { createSuggestion, SuggestionKind } from "../../domains/task-suggestion";

storiesOf("Project/Suggestion Modal", module)
  .addParameters({ info: { inline: true } })
  .add(
    "overwrite suggestion",
    () => {
      const root = document.getElementById("story-root");
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
            focusedSuggestion: 0,
            suggestions: [
              createSuggestion({
                kind: SuggestionKind.Overwrite,
                nodeName: "node",
              }),
            ],
          }}
        />
      );
    },
    { decorators: [withInfo, withKnobs] }
  )
  .add(
    "rename suggention",
    () => {
      const root = document.getElementById("story-root");
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
            focusedSuggestion: 0,
            suggestions: [
              createSuggestion({
                kind: SuggestionKind.Rename,
                nodeName: "node",
              }),
            ],
          }}
        />
      );
    },
    { decorators: [withInfo, withKnobs] }
  )
  .add(
    "Multiple suggestions",
    () => {
      const root = document.getElementById("story-root");
      if (!root) {
        return <span />;
      }
      const suggestions = [
        createSuggestion({
          kind: SuggestionKind.Overwrite,
          nodeName: "node",
        }),
        createSuggestion({
          kind: SuggestionKind.Rename,
          nodeName: "node",
        }),
      ];

      return (
        <SuggestionModal
          dialogRoot={root}
          opened={boolean("opened", false)}
          overlay={{}}
          container={{
            onReply: () => {},
            focusedSuggestion: 0,
            suggestions,
          }}
        />
      );
    },
    { decorators: [withInfo, withKnobs] }
  );
