import * as React from "react";
import renderer from "react-test-renderer";

import { Component as T } from "./file-list";

import { createFileItem } from "@/domains/file-item";
import { createFileStat } from "@/domains/file-stat";
import { createMode } from "@/domains/mode";
import { emptyCapability } from "@/domains/capability";

function makeNode(name: string, isDirectory = false, isSymlink = false) {
  return createFileItem({
    id: "node",
    name,
    marked: false,
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
  describe("Node List", () => {
    it("should not print before resized", () => {
      const nodes = [makeNode("file.txt")];
      const tree = renderer.create(<T items={nodes} location="loc" cursor={0} focused={false} />).toJSON();

      expect(tree).toMatchSnapshot();
    });

    it("should select a node locating same the index of cursor when focused", () => {
      const nodes = [makeNode("file.txt")];
      const tree = renderer.create(<T items={nodes} location="loc" cursor={0} focused={true} />).toJSON();

      expect(tree).toMatchSnapshot();
    });

    it("should show dummy content when nodes is empty", () => {
      const tree = renderer.create(<T items={[]} location="loc" cursor={0} focused={true} />).toJSON();

      expect(tree).toMatchSnapshot();
    });
  });
});
