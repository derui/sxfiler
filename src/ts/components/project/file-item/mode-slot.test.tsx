import * as React from "react";
import renderer from "react-test-renderer";
import { wrap } from "@/components/theme/test-util";
import { Component as T } from "./mode-slot";

import { emptyCapability, allowToRead } from "@/domains/capability";
import { emptyMode, changeOwner } from "@/domains/mode";

describe("Project", () => {
  describe("Node Item", () => {
    describe("Mode slot", () => {
      it("should print name simply", () => {
        const mode = emptyMode();
        const tree = renderer.create(wrap(<T mode={mode} isDirectory={false} isSymlink={false} />)).toJSON();

        expect(tree).toMatchSnapshot();
      });

      it("should print directory mode", () => {
        const mode = changeOwner(allowToRead(emptyCapability()))(emptyMode());
        const tree = renderer.create(wrap(<T mode={mode} isDirectory={true} isSymlink={false} />)).toJSON();

        expect(tree).toMatchSnapshot();
      });

      it("should print symlink mode", () => {
        const mode = changeOwner(allowToRead(emptyCapability()))(emptyMode());
        const tree = renderer.create(wrap(<T mode={mode} isDirectory={false} isSymlink={true} />)).toJSON();

        expect(tree).toMatchSnapshot();
      });

      it("should print symlink when directory and symlink are true", () => {
        const mode = changeOwner(allowToRead(emptyCapability()))(emptyMode());
        const tree = renderer.create(wrap(<T mode={mode} isDirectory={true} isSymlink={true} />)).toJSON();

        expect(tree).toMatchSnapshot();
      });
    });
  });
});
