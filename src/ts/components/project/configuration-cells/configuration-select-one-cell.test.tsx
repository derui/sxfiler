import { h } from "preact";

import { render, cleanup, fireEvent } from "@testing-library/preact";
import { render as renderAsText } from "preact-render-to-string";
import * as Cell from "./configuration-select-one-cell";
import { createSelectOne, createText } from "@/configurations/item-creators";
import { createItem } from "@/configurations/creators";

describe("Project", () => {
  describe("Configuration Boolean Cell", () => {
    afterEach(() => {
      cleanup();
    });

    it("render correctly", () => {
      const item = createItem({
        key: ["foo", "bar", "baz"],
        displayName: "name",
        description: "description",
        type: createSelectOne([], ""),
      });
      const tree = renderAsText(<Cell.Component item={item} onUpdated={() => {}} />);

      expect(tree).toMatchSnapshot();
    });

    it("invoke callback every event", async () => {
      const item = createItem({
        key: ["foo", "bar", "baz"],
        displayName: "name",
        description: "description",
        type: createSelectOne(
          [
            {
              value: "foo",
              toString() {
                return this.value;
              },
              display: "Option for foo",
            },
            {
              value: "bar",
              toString() {
                return this.value;
              },
              display: "Option for bar",
            },
          ],
          ""
        ),
      });

      const callback = jest.fn();
      const query = render(<Cell.Component item={item} onUpdated={callback} />);
      const input = await query.findByTestId("configuration-select-option-0");

      fireEvent.click(input);

      expect(callback).toBeCalledWith("foo");
    });

    it("do not render component if definition is not selectOne type", () => {
      const item = createItem({
        key: ["foo", "bar", "baz"],
        displayName: "name",
        description: "description",
        type: createText(""),
      });

      const callback = jest.fn();
      const query = render(<Cell.Component item={item} onUpdated={callback} />);
      const input = query.queryByTestId("configuration-select-one-cell-container");

      expect(input).toBeNull();
    });
  });
});
