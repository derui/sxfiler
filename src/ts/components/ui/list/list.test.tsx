import * as React from "react";
import renderer from "react-test-renderer";

import List from "./list";

describe("UI kit", () => {
  describe("List Item", () => {
    it("render with empty nodes", () => {
      const tree = renderer.create(<List />).toJSON();

      expect(tree).toMatchSnapshot();
    });

    it("render with single nodes", () => {
      const tree = renderer.create(<List>test</List>).toJSON();

      expect(tree).toMatchSnapshot();
    });

    it("can render with other container", () => {
      const tree = renderer.create(<List container="span">test</List>).toJSON();

      expect(tree).toMatchSnapshot();
    });

    it("render with single nodes", () => {
      const tree = renderer
        .create(
          <List>
            <span className="foo">foo</span>
            <a href="">link</a>
          </List>
        )
        .toJSON();

      expect(tree).toMatchSnapshot();
    });

    it("can pass names of class to apply style", () => {
      const tree = renderer
        .create(
          <List classes={["a", "b"]}>
            <span className="foo">foo</span>
          </List>
        )
        .toJSON();

      expect(tree).toMatchSnapshot();
    });

    it("can pass props directly", () => {
      const onClick = () => null;
      const tree = renderer
        .create(
          <List classes={["a", "b"]} id="foo" onClick={onClick}>
            <span className="foo">foo</span>
          </List>
        )
        .toJSON();

      expect(tree).toMatchSnapshot();
    });
  });
});
