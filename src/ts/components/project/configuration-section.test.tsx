import { h } from "preact";
import { render as renderAsText } from "preact-render-to-string";
import { Component } from "./configuration-section";
import { render, cleanup, fireEvent, act } from "@testing-library/preact";
import { createSection, createItem } from "@/configurations/creators";
import { createText, createNumber } from "@/configurations/item-creators";

describe("Project", () => {
  describe("Configuration section", () => {
    afterEach(() => cleanup());

    test("show section that do not have any items", () => {
      const section = createSection({
        key: ["cat", "section"],
        displayName: "test",
        description: "description",
        items: [],
      });
      const tree = renderAsText(<Component section={section} itemValueMap={{}} onUpdated={() => {}} />);

      expect(tree).toMatchSnapshot();
    });

    test("show section that have some items", () => {
      const section = createSection({
        key: ["cat", "section"],
        displayName: "test",
        description: "description",
        items: [
          createItem({
            key: ["cat", "section", "item1"],
            displayName: "item1",
            description: "description1",
            type: createText(""),
          }),
          createItem({
            key: ["cat", "section", "item2"],
            displayName: "item2",
            description: "description2",
            type: createNumber(10),
          }),
        ],
      });
      const tree = renderAsText(<Component section={section} itemValueMap={{}} onUpdated={() => {}} />);

      expect(tree).toMatchSnapshot();
    });

    test("callback function if an item updated", async () => {
      const section = createSection({
        key: ["cat", "section"],
        displayName: "test",
        description: "description",
        items: [
          createItem({
            key: ["cat", "section", "item1"],
            displayName: "item1",
            description: "description1",
            type: createText(),
          }),
          createItem({
            key: ["cat", "section", "item2"],
            displayName: "item2",
            description: "description2",
            type: createNumber(),
          }),
        ],
      });
      const callback = jest.fn();
      const query = render(<Component section={section} itemValueMap={{}} onUpdated={callback} />);

      const item1 = await query.findByTestId("configuration-text-cell-input");
      act(() => {
        fireEvent.input(item1, { target: { value: "foobar" } });
      });

      expect(callback).toBeCalledWith(section.items[0], "foobar");
    });
  });
});
