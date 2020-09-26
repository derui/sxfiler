import { h } from "preact";

import { render, cleanup, fireEvent } from "@testing-library/preact";
import { render as renderAsText } from "preact-render-to-string";
import * as Cell from "./configuration-number-cell";
import { createNumber, createBoolean } from "@/configurations/item-creators";
import { createItem, toItemKey } from "@/configurations/creators";

describe("Project", () => {
  describe("Configuration Number Cell", () => {
    afterEach(() => {
      cleanup();
    });

    it("render correctly", () => {
      const item = createItem({
        key: toItemKey(["foo", "bar", "baz"]),
        displayName: "name",
        description: "description",
        type: createNumber(0),
      });
      const tree = renderAsText(<Cell.Component item={item} value={100} onUpdated={() => {}} />);

      expect(tree).toMatchSnapshot();
    });

    it("invoke callback every event", async () => {
      const item = createItem({
        key: toItemKey(["foo", "bar", "baz"]),
        displayName: "name",
        description: "description",
        type: createNumber(0),
      });

      const callback = jest.fn();
      const query = render(<Cell.Component item={item} onUpdated={callback} />);
      const input = await query.findByTestId("configuration-number-cell-input");

      fireEvent.input(input, { target: { value: "100" } });

      expect(callback).toBeCalledWith("100");
    });

    it("do not invoke callback when input is not valid integer", async () => {
      const item = createItem({
        key: toItemKey(["foo", "bar", "baz"]),
        displayName: "name",
        description: "description",
        type: createNumber(0),
      });

      const callback = jest.fn();
      const query = render(<Cell.Component item={item} onUpdated={callback} />);
      const input = await query.findByTestId("configuration-number-cell-input");

      fireEvent.input(input, { target: { value: "10.0" } });

      expect(callback).toBeCalledTimes(0);
    });

    it("do not render component when definition is not number type", () => {
      const item = createItem({
        key: toItemKey(["foo", "bar", "baz"]),
        displayName: "name",
        description: "description",
        type: createBoolean(false),
      });

      const callback = jest.fn();
      const query = render(<Cell.Component item={item} onUpdated={callback} />);
      const input = query.queryByTestId("configuration-number-cell-input");

      expect(input).toBeNull();
    });
  });
});
