import { h } from "preact";
import { render } from "preact-render-to-string";
import { Component as T } from "./name-slot";

describe("Project", () => {
  describe("Node Item", () => {
    describe("Name slot", () => {
      it("should print name simply", () => {
        const tree = render(<T name="name" isDirectory={false} isSymlink={false} />);

        expect(tree).toMatchSnapshot();
      });

      it("should print name as directory", () => {
        const tree = render(<T name="name" isDirectory={true} isSymlink={false} />);

        expect(tree).toMatchSnapshot();
      });

      it("should print name as symlink", () => {
        const tree = render(<T name="name" isDirectory={false} isSymlink={true} />);

        expect(tree).toMatchSnapshot();
      });

      it("should print name as symlink when directory and symlink flag are enabled", () => {
        const tree = render(<T name="name" isDirectory={true} isSymlink={true} />);

        expect(tree).toMatchSnapshot();
      });
    });
  });
});
