import { withInfo } from "@storybook/addon-info";
import { boolean, withKnobs } from "@storybook/addon-knobs";
import { storiesOf } from "@storybook/react";
import * as React from "react";

import { create } from "../../domains/node";

import { Component as NodeItem } from "../../components/project/node-item/node-item";
import FileStatFactory from "../../domains/file-stat-factory";

storiesOf("Project/Node Item", module)
  .addDecorator(withInfo)
  .addDecorator(withKnobs)
  .addParameters({ info: { inline: true } })
  .add("simple file node", () => {
    const item = create({
      id: "node",
      name: "file.txt",
      marked: false,
      stat: FileStatFactory.create({
        mode: "644",
        uid: 1000,
        gid: 1000,
        atime: "0",
        ctime: "0",
        mtime: "0",
        size: "10",
        isDirectory: false,
        isFile: true,
        isSymlink: false,
      }),
      parentDirectory: "/",
    });
    return <NodeItem item={item} selected={boolean("Selected", false)} />;
  })
  .add("simple directory node", () => {
    const item = create({
      id: "node",
      name: "file",
      marked: false,
      stat: FileStatFactory.create({
        mode: "755",
        uid: 1000,
        gid: 1000,
        atime: "0",
        ctime: "0",
        mtime: "0",
        size: "10",
        isDirectory: true,
        isFile: false,
        isSymlink: false,
      }),
      parentDirectory: "/",
    });
    return <NodeItem item={item} selected={boolean("Selected", false)} />;
  })
  .add("simple symlink node", () => {
    const item = create({
      id: "node",
      name: "file",
      marked: false,
      stat: FileStatFactory.create({
        mode: "755",
        uid: 1000,
        gid: 1000,
        atime: "0",
        ctime: "0",
        mtime: "0",
        size: "10",
        isDirectory: false,
        isFile: false,
        isSymlink: true,
      }),
      parentDirectory: "/",
    });
    return <NodeItem item={item} selected={boolean("Selected", false)} />;
  })
  .add("marked node", () => {
    const item = create({
      id: "node",
      name: "file",
      marked: true,
      stat: FileStatFactory.create({
        mode: "755",
        uid: 1000,
        gid: 1000,
        atime: "0",
        ctime: "0",
        mtime: "0",
        size: "10",
        isDirectory: false,
        isFile: false,
        isSymlink: true,
      }),
      parentDirectory: "/",
    });
    return <NodeItem item={item} selected={boolean("Selected", false)} />;
  });
