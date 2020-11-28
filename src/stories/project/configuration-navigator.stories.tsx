import { withInfo } from "@storybook/addon-info";
import { withKnobs } from "@storybook/addon-knobs";
import { storiesOf } from "@storybook/preact";
import { h } from "preact";

import { Component } from "@/components/project/configuration-navigator";
import { createCategory, createSection } from "@/configurations/creators";

storiesOf("Project/Configuration Navigator", module)
  .addParameters({ info: { inline: true } })
  .add(
    "single category",
    () => {
      const categories = [
        createCategory({
          key: ["key"],
          displayName: "category1",
          description: "description",
          sections: [
            createSection({
              key: ["key", "section1"],
              displayName: "section1",
              description: "description",
              items: [],
            }),
            createSection({
              key: ["key", "section2"],
              displayName: "section2",
              description: "description",
              items: [],
            }),
          ],
        }),
      ];
      return (
        <div class="theme__default">
          <Component categories={categories} onSectionSelected={() => {}} />
        </div>
      );
    },
    { decorators: [withInfo, withKnobs] }
  )
  .add(
    "multiple category",
    () => {
      const categories = [
        createCategory({
          key: ["key"],
          displayName: "category1",
          description: "description",
          sections: [
            createSection({
              key: ["key", "section1"],
              displayName: "section1",
              description: "description",
              items: [],
            }),
            createSection({
              key: ["key", "section2"],
              displayName: "section2",
              description: "description",
              items: [],
            }),
          ],
        }),
        createCategory({
          key: ["key2"],
          displayName: "category2",
          description: "description",
          sections: [
            createSection({
              key: ["key2", "section1"],
              displayName: "section1",
              description: "description",
              items: [],
            }),
            createSection({
              key: ["key2", "section2"],
              displayName: "section2",
              description: "description",
              items: [],
            }),
          ],
        }),
      ];
      return (
        <div class="theme__default">
          <Component categories={categories} onSectionSelected={() => {}} />
        </div>
      );
    },
    { decorators: [withInfo, withKnobs] }
  );
