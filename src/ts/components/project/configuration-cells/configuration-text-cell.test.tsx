import { h } from "preact";

import { render, cleanup, fireEvent } from "@testing-library/preact";
import { render as renderAsText } from "preact-render-to-string";
import { Component as Cell } from "./configuration-text-cell";
import { createText, createNumber } from "@/configurations/item-creators";
import { createItem } from "@/configurations/creators";

describe("Project", () => {
  describe("Configuration Text Cell", () => {
    afterEach(() => {
      cleanup();
    });

    it("render correctly", () => {
      const item = createItem({
        key: ["foo", "bar", "baz"],
        description: "description",
        displayName: "display",
        type: createText(""),
      });
      const tree = renderAsText(<Cell item={item} onUpdated={() => {}} />);

      expect(tree).toMatchSnapshot();
    });

    it("invoke callback every event", async () => {
      const item = createItem({
        key: ["foo", "bar", "baz"],
        description: "description",
        displayName: "display",
        type: createText(""),
      });

      const callback = jest.fn();
      const query = render(<Cell item={item} onUpdated={callback} />);
      const input = await query.findByTestId("configuration-text-cell-input");

      fireEvent.input(input, { target: { value: "foo" } });

      expect(callback).toBeCalledWith("foo");
    });

    it("do not render component if definition is not text type", () => {
      const item = createItem({
        key: ["foo", "bar", "baz"],
        description: "description",
        displayName: "display",
        type: createNumber(0),
      });

      const callback = jest.fn();
      const query = render(<Cell item={item} onUpdated={callback} />);
      const input = query.queryByTestId("configuration-text-cell-input");

      expect(input).toBeNull();
    });
  });
});
