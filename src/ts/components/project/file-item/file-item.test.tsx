import * as React from "react";
import renderer from "react-test-renderer";

import { createFileItem } from "../../../domains/file-item";

import { Component as T } from "./file-item";
import { createMode } from "../../../domains/mode";
import { emptyCapability } from "../../../domains/capability";
import { createFileStat } from "../../../domains/file-stat";

function makeNode(marked: boolean, isDirectory = false, isSymlink = false) {
  return createFileItem({
    id: "node",
    name: "file.ext",
    marked,
    stat: createFileStat({
      mode: createMode({
        owner: emptyCapability()
          .allowToRead()
          .allowToWrite(),
        group: emptyCapability().allowToRead(),
        others: emptyCapability().allowToRead(),
      }),
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
