import * as React from "react";
import renderer from "react-test-renderer";

import { create } from "../../../domains/node";

import FileStatFactory from "../../../domains/file-stat-factory";
import { Component as T } from "./node-item";

function makeNode(marked: boolean, isDirectory = false, isSymlink = false) {
  return create({
    id: "node",
    name: "file.ext",
    marked,
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
    parentDirectory: "/",
  });
}

describe("Project", () => {
  describe("Node Item", () => {
    it("should print correctly", () => {
      const node = makeNode(false);
      const tree = renderer.create(<T item={node} selected={false} />).toJSON();

      expect(tree).toMatchSnapshot();
    });

    it("should add class correctly when marked", () => {
      const node = makeNode(true);
      const tree = renderer.create(<T item={node} selected={false} />).toJSON();

      expect(tree).toMatchSnapshot();
    });

    it("should add class correctly when selected", () => {
      const node = makeNode(false);
      const tree = renderer.create(<T item={node} selected={true} />).toJSON();

      expect(tree).toMatchSnapshot();
    });

    it("should add class correctly when selected and marked", () => {
      const node = makeNode(true);
      const tree = renderer.create(<T item={node} selected={true} />).toJSON();

      expect(tree).toMatchSnapshot();
    });
  });
});
