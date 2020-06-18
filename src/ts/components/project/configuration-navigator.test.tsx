import { h } from "preact";
import { render as renderAsText } from "preact-render-to-string";
import { Component } from "./configuration-navigator";
import { render, cleanup, fireEvent, act } from "@testing-library/preact";
import { createSection, createCategory } from "@/configurations/creators";

describe("Project", () => {
  describe("Configuration navigator", () => {
    afterEach(() => cleanup());

    test("show empty categories", () => {
      const tree = renderAsText(<Component categories={[]} onSectionSelected={() => {}} />);

      expect(tree).toMatchSnapshot();
    });

    test("render category and sections as tree", async () => {
      const category = createCategory({
        key: ["key"],
        displayName: "display",
        description: "desc",
        sections: [
          createSection({
            key: ["key", "key2"],
            displayName: "section",
            description: "desc",
            items: [],
          }),
        ],
      });
      const query = render(<Component categories={[category]} onSectionSelected={() => {}} />);

      const section = await query.findAllByTestId("configuration-navigator-section");

      expect(section).toHaveLength(1);
      expect(section[0]).toHaveTextContent("section");
    });

    test("open hidden sections when category is clicked", async () => {
      const category = createCategory({
        key: ["key"],
        displayName: "display",
        description: "desc",
        sections: [
          createSection({
            key: ["key", "key2"],
            displayName: "section",
            description: "desc",
            items: [],
          }),
        ],
      });
      const query = render(<Component categories={[category]} onSectionSelected={() => {}} />);

      const sectionTree = await query.findByTestId("configuration-navigator-section-tree");

      expect(sectionTree).toHaveAttribute("aria-hidden", "true");

      const categoryDom = await query.findByTestId("configuration-navigator-category-name");
      act(() => {
        fireEvent.click(categoryDom);
      });

      expect(sectionTree).toHaveAttribute("aria-hidden", "false");
    });

    test("callback function when seciton is clicked", async () => {
      const category = createCategory({
        key: ["key"],
        displayName: "display",
        description: "desc",
        sections: [
          createSection({
            key: ["key", "key2"],
            displayName: "section",
            description: "desc",
            items: [],
          }),
        ],
      });
      const callback = jest.fn();
      const query = render(<Component categories={[category]} onSectionSelected={callback} />);

      const categoryDom = await query.findByTestId("configuration-navigator-category-name");
      act(() => {
        fireEvent.click(categoryDom);
      });
      const sectionDom = await query.findByTestId("configuration-navigator-section");
      act(() => {
        fireEvent.click(sectionDom);
      });

      expect(callback).toBeCalledWith(category, category.sections[0]);
    });
  });
});
