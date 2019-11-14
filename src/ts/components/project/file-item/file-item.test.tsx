import * as React from "react";
import renderer from "react-test-renderer";

import { createFileItem } from "@/domains/file-item";
import { Theme, ThemeProvider } from "@/components/theme";

import { Component as T } from "./file-item";
import { createMode } from "@/domains/mode";
import { emptyCapability, allowToRead, allowToWrite } from "@/domains/capability";
import { createFileStat } from "@/domains/file-stat";
import { pipe } from "@/libs/fn";

function makeNode(marked: boolean, isDirectory = false, isSymlink = false) {
  return createFileItem({
    id: "node",
    name: "file.ext",
    marked,
    fullPath: "/file.ext",
    stat: createFileStat({
      mode: createMode({
        owner: pipe(allowToRead, allowToWrite)(emptyCapability()),
        group: allowToRead(emptyCapability()),
        others: allowToRead(emptyCapability()),
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

function wrap(comp: React.ReactElement) {
  return <ThemeProvider theme={Theme}>{comp}</ThemeProvider>;
}

describe("Project", () => {
  describe("Node Item", () => {
    it("should print correctly", () => {
      const node = makeNode(false);
      const tree = renderer.create(wrap(<T item={node} selected={false} bookmarked={false} />)).toJSON();

      expect(tree).toMatchSnapshot();
    });

    it("should add class correctly when marked", () => {
      const node = makeNode(true);
      const tree = renderer.create(wrap(<T item={node} selected={false} bookmarked={false} />)).toJSON();

      expect(tree).toMatchSnapshot();
    });

    it("should add class correctly when selected", () => {
      const node = makeNode(false);
      const tree = renderer.create(wrap(<T item={node} selected={true} bookmarked={false} />)).toJSON();

      expect(tree).toMatchSnapshot();
    });

    it("should add class correctly when selected and marked", () => {
      const node = makeNode(true);
      const tree = renderer.create(wrap(<T item={node} selected={true} bookmarked={false} />)).toJSON();

      expect(tree).toMatchSnapshot();
    });

    it("should add class correctly when it bookmarked", () => {
      const node = makeNode(true);
      const tree = renderer.create(wrap(<T item={node} selected={true} bookmarked={true} />)).toJSON();

      expect(tree).toMatchSnapshot();
    });
  });
});
