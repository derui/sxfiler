import * as React from "react";
import renderer from "react-test-renderer";
import T from "./mode-slot";

import { emptyCapability } from "@/domains/capability";
import { emptyMode } from "@/domains/mode";

describe("Project", () => {
  describe("Node Item", () => {
    describe("Mode slot", () => {
      it("should print name simply", () => {
        const mode = emptyMode();
        const tree = renderer.create(<T mode={mode} isDirectory={false} isSymlink={false} />).toJSON();

        expect(tree).toMatchSnapshot();
      });

      it("should print directory mode", () => {
        const mode = emptyMode().changeOwner(emptyCapability().allowToRead());
        const tree = renderer.create(<T mode={mode} isDirectory={true} isSymlink={false} />).toJSON();

        expect(tree).toMatchSnapshot();
      });

      it("should print symlink mode", () => {
        const mode = emptyMode().changeOwner(emptyCapability().allowToRead());
        const tree = renderer.create(<T mode={mode} isDirectory={false} isSymlink={true} />).toJSON();

        expect(tree).toMatchSnapshot();
      });

      it("should print symlink when directory and symlink are true", () => {
        const mode = emptyMode().changeOwner(emptyCapability().allowToRead());
        const tree = renderer.create(<T mode={mode} isDirectory={true} isSymlink={true} />).toJSON();

        expect(tree).toMatchSnapshot();
      });
    });
  });
});
