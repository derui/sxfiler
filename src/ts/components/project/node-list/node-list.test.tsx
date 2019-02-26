import * as React from "react";
import { shallow } from "enzyme";
import renderer from "react-test-renderer";

import NodeItem from "../node-item/node-item";
import T from "./node-list";

import { create } from "../../../domains/node";

import FileStatFactory from "../../../domains/file-stat-factory";

function makeNode(name: string, isDirectory = false, isSymlink = false) {
  return create({
    id: "node",
    name,
    marked: false,
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
  describe("Node List", () => {
    it("should print correctly", () => {
      const nodes = [makeNode("file.txt")];
      const wrapper = shallow(<T nodes={nodes} location="loc" cursor={0} focused={false} />)

      expect(wrapper.find(NodeItem)).toHaveLength(1);
    });

    it("should select a node locating same the index of cursor when focused", () => {
      const nodes = [makeNode("file.txt")];
      const tree = renderer.create(<T nodes={nodes} location="loc" cursor={0} focused={true} />).toJSON();

      expect(tree).toMatchSnapshot();
    });

    it("should show dummy content when nodes is empty", () => {
      const tree = renderer.create(<T nodes={[]} location="loc" cursor={0} focused={true} />).toJSON();

      expect(tree).toMatchSnapshot();
    });
  });
});
