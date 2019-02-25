import * as React from "react";
import renderer from "react-test-renderer";

import * as Element from "./element";

describe("UI kit", () => {
  describe("Element", () => {
    it("render correctly with no child", () => {
      const E = Element.createComponent();
      const tree = renderer.create(<E />).toJSON();

      expect(tree).toMatchSnapshot();
    });

    it("render correctly with className", () => {
      const E = Element.createComponent();
      const tree = renderer.create(<E className="base" />).toJSON();

      expect(tree).toMatchSnapshot();
    });

    it("render with data and aria", () => {
      const E = Element.createComponent();
      const tree = renderer.create(<E className="base" data-test="next" aria-hidden={true} />).toJSON();

      expect(tree).toMatchSnapshot();
    });

    it("render element as specified tag name", () => {
      const E = Element.createComponent({ tagName: "span" });
      const tree = renderer.create(<E className="base" />).toJSON();

      expect(tree).toMatchSnapshot();
    });

    it("render element with content", () => {
      const E = Element.createComponent();
      const tree = renderer.create(<E>test</E>).toJSON();

      expect(tree).toMatchSnapshot();
    });
  });
});
