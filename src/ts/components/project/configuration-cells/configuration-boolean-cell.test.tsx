import { h } from "preact";

import { render, cleanup, fireEvent, act } from "@testing-library/preact";
import { render as renderAsText } from "preact-render-to-string";
import { Component as Cell } from "./configuration-boolean-cell";
import { createItem, toItemKey } from "@/configurations/creators";
import { createBoolean, createNumber } from "@/configurations/item-creators";

describe("Project", () => {
  describe("Configuration Boolean Cell", () => {
    afterEach(() => {
      cleanup();
    });

    it("render correctly", () => {
      const item = createItem({
        key: toItemKey(["foo", "bar", "baz"]),
        displayName: "name",
        description: "description",
        type: createBoolean(false),
      });
      const tree = renderAsText(<Cell item={item} value={true} onUpdated={() => {}} />);

      expect(tree).toMatchSnapshot();
    });

    it("invoke callback every event", async () => {
      const item = createItem({
        key: toItemKey(["foo", "bar", "baz"]),
        displayName: "name",
        description: "description",
        type: createBoolean(false),
      });

      const callback = jest.fn();
      const query = render(<Cell item={item} onUpdated={callback} />);
      const input = await query.findByTestId("switch-root");

      act(() => {
        fireEvent.click(input);
      });

      expect(callback).toBeCalledWith(true);
    });

    it("do not render component if definition is not boolean type", () => {
      const item = createItem({
        key: toItemKey(["foo", "bar", "baz"]),
        displayName: "name",
        description: "description",
        type: createNumber(10),
      });

      const callback = jest.fn();
      const query = render(<Cell item={item} onUpdated={callback} />);
      const input = query.queryByTestId("configuration-boolean-cell-container");

      expect(input).toBeNull();
    });
  });
});
