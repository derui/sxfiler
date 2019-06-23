import * as React from "react";
import renderer from "react-test-renderer";
import T from "./name-slot";

describe("Project", () => {
  describe("Node Item", () => {
    describe("Name slot", () => {
      it("should print name simply", () => {
        const tree = renderer.create(<T name="name" isDirectory={false} isSymlink={false} />).toJSON();

        expect(tree).toMatchSnapshot();
      });

      it("should print name as directory", () => {
        const tree = renderer.create(<T name="name" isDirectory={true} isSymlink={false} />).toJSON();

        expect(tree).toMatchSnapshot();
      });

      it("should print name as symlink", () => {
        const tree = renderer.create(<T name="name" isDirectory={false} isSymlink={true} />).toJSON();

        expect(tree).toMatchSnapshot();
      });

      it("should print name as symlink when directory and symlink flag are enabled", () => {
        const tree = renderer.create(<T name="name" isDirectory={true} isSymlink={true} />).toJSON();

        expect(tree).toMatchSnapshot();
      });
    });
  });
});
