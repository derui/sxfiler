import * as React from "react";
import renderer from "react-test-renderer";

import { create } from "../../../domains/node";

import T from "./node-item";
import FileStatFactory from "../../../domains/file-stat-factory";

function makeNode(isDirectory = false, isSymlink = false) {
  return create({
    id: "node",
    name: "file.ext",
    stat: FileStatFactory.create({
      mode: "644",
      uid: 1000,
      gid: 1000,
      atime: "0",
      ctime: "0",
      mtime: "0",
      size: "10",
      isDirectory: isDirectory,
      isFile: !isDirectory && !isSymlink,
      isSymlink: isSymlink,
    }),
    parentDirectory: "/"
  });
}

describe("Project", () => {
  describe("Node Item", () => {
    it("should print correctly", () => {
      const node = makeNode();
      const tree = renderer.create(<T item={node} marked={false} selected={false} />).toJSON();

      expect(tree).toMatchSnapshot();
    });

    it("should add class correctly when marked", () => {
      const node = makeNode();
      const tree = renderer.create(<T item={node} marked={true} selected={false} />).toJSON();

      expect(tree).toMatchSnapshot();
    });

    it("should add class correctly when selected", () => {
      const node = makeNode();
      const tree = renderer.create(<T item={node} marked={false} selected={true} />).toJSON();

      expect(tree).toMatchSnapshot();
    });

    it("should add class correctly when selected and marked", () => {
      const node = makeNode();
      const tree = renderer.create(<T item={node} marked={true} selected={true} />).toJSON();

      expect(tree).toMatchSnapshot();
    });
  });
});
