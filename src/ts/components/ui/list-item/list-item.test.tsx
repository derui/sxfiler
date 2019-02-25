import * as React from "react";
import renderer from "react-test-renderer";

import * as ListItem from "./list-item";

describe("UI kit", () => {
  describe("List Item", () => {
    it("render with empty nodes", () => {
      const C = ListItem.createComponent();
      const tree = renderer.create(<C />).toJSON();

      expect(tree).toMatchSnapshot();
    });

    it("render with single nodes", () => {
      const C = ListItem.createComponent();
      const tree = renderer.create(<C>test</C>).toJSON();

      expect(tree).toMatchSnapshot();
    });

    it("render with single nodes", () => {
      const C = ListItem.createComponent();
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
      const C = ListItem.createComponent();
      const tree = renderer
        .create(
          <C className="item">
            <span className="foo">foo</span>
          </C>
        )
        .toJSON();

      expect(tree).toMatchSnapshot();
    });

    it("can pass props directly", () => {
      const C = ListItem.createComponent();
      const onClick = () => null;
      const tree = renderer
        .create(
          <C selected={false} onClick={onClick}>
            <span className="foo">foo</span>
          </C>
        )
        .toJSON();

      expect(tree).toMatchSnapshot();
    });
  });
});
