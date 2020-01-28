import { withInfo } from "@storybook/addon-info";
import { number, withKnobs, boolean } from "@storybook/addon-knobs";
import { storiesOf } from "@storybook/preact";
import { h } from "preact";
import { Component as Completer } from "@/components/project/completer";

const style = {
  height: "100px",
};

const createCandidate = (obj: { id: string; value: string; start: number; length: number }) => {
  return {
    id: obj.id,
    before: "",
    after: "",
    matched: "",
  };
};

storiesOf("Project/Completer", module)
  .addParameters({ info: { inline: true } })
  .add(
    "empty completion",
    () => {
      const root = document.getElementById("modal-root");
      if (!root) {
        return <span />;
      }
      return (
        <div class="theme__default" style={style}>
          <Completer
            dialogRoot={root}
            opened={boolean("opened", false)}
            title={"completion"}
            items={[]}
            selectedItemIndex={number("selected item", 0)}
            onInput={() => {}}
          />
        </div>
      );
    },
    { decorators: [withInfo, withKnobs] }
  )
  .add(
    "with some items",
    () => {
      const root = document.getElementById("modal-root");
      if (!root) {
        return <span />;
      }
      const items = [
        createCandidate({ id: "id1", value: "foo", start: 0, length: 0 }),
        createCandidate({ id: "id2", value: "foobar", start: 3, length: 3 }),
        createCandidate({ id: "id3", value: "barbaz", start: 0, length: 3 }),
      ];

      return (
        <div class="theme__default" style={style}>
          <Completer
            dialogRoot={root}
            opened={true}
            title={"completion"}
            items={items}
            selectedItemIndex={number("item", 0)}
            onInput={() => {}}
          />
        </div>
      );
    },
    { decorators: [withInfo, withKnobs] }
  );
