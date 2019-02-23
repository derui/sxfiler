import * as React from "react";
import renderer from "react-test-renderer";

import ListItem from "./list-item";

describe("UI kit", () => {
  describe("List Item", () => {
    it("render with empty nodes", () => {
      const tree = renderer.create(<ListItem />).toJSON();

      expect(tree).toMatchSnapshot();
    });

    it("render with single nodes", () => {
      const tree = renderer.create(<ListItem>test</ListItem>).toJSON();

      expect(tree).toMatchSnapshot();
    });

    it("render as specified component", () => {
      const tree = renderer.create(<ListItem container="a">test</ListItem>).toJSON();

      expect(tree).toMatchSnapshot();
    });

    it("render with single nodes", () => {
      const tree = renderer
        .create(
          <ListItem>
            <span className="foo">foo</span>
            <a href="">link</a>
          </ListItem>
        )
        .toJSON();

      expect(tree).toMatchSnapshot();
    });

    it("can pass names of class to apply style", () => {
      const tree = renderer
        .create(
          <ListItem classes={["a", "b"]}>
            <span className="foo">foo</span>
          </ListItem>
        )
        .toJSON();

      expect(tree).toMatchSnapshot();
    });

    it("can pass props directly", () => {
      const onClick = () => null;
      const tree = renderer
        .create(
          <ListItem classes={["a", "b"]} hidden={true} onClick={onClick}>
            <span className="foo">foo</span>
          </ListItem>
        )
        .toJSON();

      expect(tree).toMatchSnapshot();
    });
  });
});
