import { h } from "preact";
import { render } from "preact-render-to-string";

import { Component as T } from "./index";

import { also } from "@/libs/fn";
import { FileItem, FileStat, Mode, Capability } from "@/generated/filer_pb";
import { Bookmark } from "@/generated/bookmark_pb";

function makeNode(name: string) {
  const item = new FileItem();
  item.setName(name);
  item.setMarked(true);
  item.setStat(
    also(new FileStat(), (v) => {
      v.setIsDirectory(false);
      v.setIsSymlink(false);
      v.setIsFile(true);
      v.setMode(
        also(new Mode(), (v) => {
          v.setOwner(
            also(new Capability(), (v) => {
              v.setReadable(true);
              v.setWritable(true);
            })
          );
        })
      );
      v.setAtime("0");
      v.setCtime("0");
      v.setMtime("0");
      v.setSize("10");
    })
  );
  return item;
}

const bookmarks = [
  also(new Bookmark(), (v) => {
    v.setName("bookmark");
    v.setPath("fullpath");
  }).toObject(),
];

describe("Project", () => {
  describe("Node List", () => {
    it("should not print before resized", () => {
      const nodes = [makeNode("file.txt").toObject()];
      const tree = render(<T items={nodes} location="loc" cursor={0} focused={false} bookmarks={bookmarks} />);

      expect(tree).toMatchSnapshot();
    });

    it("should select a node locating same the index of cursor when focused", () => {
      const nodes = [makeNode("file.txt").toObject()];
      const tree = render(<T items={nodes} location="loc" cursor={0} focused={true} bookmarks={bookmarks} />);

      expect(tree).toMatchSnapshot();
    });

    it("should show dummy content when nodes is empty", () => {
      const tree = render(<T items={[]} location="loc" cursor={0} focused={true} bookmarks={bookmarks} />);

      expect(tree).toMatchSnapshot();
    });
  });
});
