import * as React from "react";
import renderer from "react-test-renderer";

import * as Element from "@/components/ui/element/element";
import * as List from "./list";

describe("UI kit", () => {
  describe("List Item", () => {
    it("render with empty nodes", () => {
      const C = List.createComponent();
      const tree = renderer.create(<C />).toJSON();

      expect(tree).toMatchSnapshot();
    });

    it("render with single nodes", () => {
      const C = List.createComponent();
      const tree = renderer.create(<C>test</C>).toJSON();

      expect(tree).toMatchSnapshot();
    });

    it("can render with other container", () => {
      const E = Element.createComponent({ tagName: "span" });
      const C = List.createComponent({ container: E });
      const tree = renderer.create(<C>test</C>).toJSON();

      expect(tree).toMatchSnapshot();
    });

    it("render with single nodes", () => {
      const C = List.createComponent();
      const tree = renderer
        .create(
          <C>
            <span className="foo">foo</span>
            <a href="">link</a>
          </C>
        )
        .toJSON();

      expect(tree).toMatchSnapshot();
    });

    it("can pass names of class to apply style", () => {
      const C = List.createComponent();
      const tree = renderer
        .create(
          <C className="a b">
            <span className="foo">foo</span>
          </C>
        )
        .toJSON();

      expect(tree).toMatchSnapshot();
    });

    it("can pass props directly", () => {
      const C = List.createComponent();
      const onClick = () => null;
      const tree = renderer
        .create(
          <C className="a b" id="foo" onClick={onClick}>
            <span className="foo">foo</span>
          </C>
        )
        .toJSON();

      expect(tree).toMatchSnapshot();
    });
  });
});
