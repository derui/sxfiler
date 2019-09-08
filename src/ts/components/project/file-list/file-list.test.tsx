import * as React from "react";
import renderer from "react-test-renderer";
import { wrap } from "@/components/theme/test-util";

import { Component as T } from "./file-list";

import { createFileItem } from "@/domains/file-item";
import { createFileStat } from "@/domains/file-stat";
import { createMode } from "@/domains/mode";
import { emptyCapability, allowToRead, allowToWrite } from "@/domains/capability";
import { pipe } from "@/libs/fn";
import { createBookmark } from "@/domains/bookmark";

function makeNode(name: string, isDirectory = false, isSymlink = false) {
  return createFileItem({
    id: "node",
    name,
    marked: false,
    fullPath: "fullpath",
    stat: createFileStat({
      mode: createMode({
        owner: pipe(
          allowToRead,
          allowToWrite
        )(emptyCapability()),
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

const bookmarks = [createBookmark({ id: "bookmark", order: 1, path: "fullpath" })];

describe("Project", () => {
  describe("Node List", () => {
    it("should not print before resized", () => {
      const nodes = [makeNode("file.txt")];
      const tree = renderer
        .create(wrap(<T items={nodes} location="loc" cursor={0} focused={false} bookmarks={bookmarks} />))
        .toJSON();

      expect(tree).toMatchSnapshot();
    });

    it("should select a node locating same the index of cursor when focused", () => {
      const nodes = [makeNode("file.txt")];
      const tree = renderer
        .create(wrap(<T items={nodes} location="loc" cursor={0} focused={true} bookmarks={bookmarks} />))
        .toJSON();

      expect(tree).toMatchSnapshot();
    });

    it("should show dummy content when nodes is empty", () => {
      const tree = renderer
        .create(wrap(<T items={[]} location="loc" cursor={0} focused={true} bookmarks={bookmarks} />))
        .toJSON();

      expect(tree).toMatchSnapshot();
    });
  });
});
