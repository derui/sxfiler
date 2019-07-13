import * as React from "react";
import { wrap } from "@/components/theme/test-util";
import renderer from "react-test-renderer";
import { Component } from "./overwrite-suggestion-panel";

describe("Project", () => {
  describe("Overwrite suggestion", () => {
    it("render with overwrite suggestion", () => {
      const tree = renderer.create(wrap(<Component selected={false} />)).toJSON();

      expect(tree).toMatchSnapshot();
    });

    it("change style when selected", () => {
      const tree = renderer.create(wrap(<Component selected={true} />)).toJSON();

      expect(tree).toMatchSnapshot();
    });
  });
});
