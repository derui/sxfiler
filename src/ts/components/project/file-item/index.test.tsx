import { h } from "preact";
import { render } from "preact-render-to-string";

import { Component as T } from "./index";
import { also } from "@/libs/fn";
import { FileItem, FileStat, Mode, Capability } from "@/generated/filer_pb";

function makeNode(marked: boolean, isDirectory = false, isSymlink = false) {
  const item = new FileItem();
  item.setName("file.ext");
  item.setMarked(marked);
  item.setStat(
    also(new FileStat(), (v) => {
      v.setIsDirectory(isDirectory);
      v.setIsSymlink(isSymlink);
      v.setIsFile(!isDirectory && !isSymlink);
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

describe("Project", () => {
  describe("Node Item", () => {
    it("should print correctly", () => {
      const node = makeNode(false).toObject();
      const tree = render(<T item={node} selected={false} bookmarked={false} />);

      expect(tree).toMatchSnapshot();
    });

    it("should add class correctly when marked", () => {
      const node = makeNode(true).toObject();
      const tree = render(<T item={node} selected={false} bookmarked={false} />);

      expect(tree).toMatchSnapshot();
    });

    it("should add class correctly when selected", () => {
      const node = makeNode(false).toObject();
      const tree = render(<T item={node} selected={true} bookmarked={false} />);

      expect(tree).toMatchSnapshot();
    });

    it("should add class correctly when selected and marked", () => {
      const node = makeNode(true).toObject();
      const tree = render(<T item={node} selected={true} bookmarked={false} />);

      expect(tree).toMatchSnapshot();
    });

    it("should add class correctly when it bookmarked", () => {
      const node = makeNode(true).toObject();
      const tree = render(<T item={node} selected={true} bookmarked={true} />);

      expect(tree).toMatchSnapshot();
    });
  });
});
