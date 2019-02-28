import { withInfo } from "@storybook/addon-info";
import { boolean, number, withKnobs } from "@storybook/addon-knobs";
import { storiesOf } from "@storybook/react";
import * as React from "react";

import { create } from "../../domains/node";

import NodeList from "../../components/project/node-list/node-list";
import FileStatFactory from "../../domains/file-stat-factory";

function makeNode(name: string, marked: boolean, isDirectory = false, isSymlink = false) {
  return create({
    id: "node",
    name,
    marked: marked,
    stat: FileStatFactory.create({
      mode: "644",
      uid: 1000,
      gid: 1000,
      atime: "0",
      ctime: "0",
      mtime: "0",
      size: "10",
      isDirectory,
      isFile: !isDirectory && !isSymlink,
      isSymlink,
    }),
    parentDirectory: "/",
  });
}

storiesOf("Project/Node List", module)
  .addDecorator(withInfo)
  .addDecorator(withKnobs)
  .addParameters({ info: { inline: true } })
  .add("empty list", () => {
    return <NodeList nodes={[]} location="loc" cursor={number("cursor", 0)} focused={boolean("focused", false)} />;
  })
  .add("with some nodes", () => {
    const nodes = [makeNode("file.txt", true), makeNode("dir", false, true), makeNode("link.txt", false, false, false)];
    return <NodeList nodes={nodes} location="loc" cursor={number("cursor", 0)} focused={boolean("focused", false)} />;
  });
