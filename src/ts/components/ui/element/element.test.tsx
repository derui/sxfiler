import * as React from "react";
import renderer from "react-test-renderer";

import { Element as E } from "./element";

describe("UI kit", () => {
  describe("Element", () => {
    it("render correctly with no child", () => {
      const tree = renderer.create(<E />).toJSON();

      expect(tree).toMatchSnapshot();
    });

    it("render correctly with className", () => {
      const tree = renderer.create(<E className="base" />).toJSON();

      expect(tree).toMatchSnapshot();
    });

    it("render without classes", () => {
      const tree = renderer.create(<E className="base" classes={{ test: "next" }} />).toJSON();

      expect(tree).toMatchSnapshot();
    });

    it("render with data and aria", () => {
      const tree = renderer.create(<E className="base" data={{ test: "next" }} aria={{ hidden: true }} />).toJSON();

      expect(tree).toMatchSnapshot();
    });

    it("render element as specified tag name", () => {
      const tree = renderer.create(<E className="base" tagName="span" />).toJSON();

      expect(tree).toMatchSnapshot();
    });
  });
});
