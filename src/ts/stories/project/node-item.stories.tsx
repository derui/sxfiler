import { withInfo } from "@storybook/addon-info";
import { boolean, withKnobs } from "@storybook/addon-knobs";
import { storiesOf } from "@storybook/react";
import * as React from "react";

import { createNode } from "../../domains/node";

import { Component as NodeItem } from "../../components/project/node-item/node-item";
import { createFileStat } from "../../domains/file-stat";
import { emptyMode, createMode } from "../../domains/mode";
import { fullCapability } from "../../domains/capability";

storiesOf("Project/Node Item", module)
  .addParameters({ info: { inline: true } })
  .add(
    "simple file node",
    () => {
      const item = createNode({
        id: "node",
        name: "file.txt",
        marked: false,
        stat: createFileStat({
          mode: emptyMode(),
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
    },
    { decorators: [withInfo, withKnobs] }
  )
  .add(
    "simple directory node",
    () => {
      const item = createNode({
        id: "node",
        name: "file",
        marked: false,
        stat: createFileStat({
          mode: createMode({
            owner: fullCapability(),
            group: fullCapability(),
            others: fullCapability().disallowToWrite(),
          }),
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
    },
    { decorators: [withInfo, withKnobs] }
  )
  .add(
    "simple symlink node",
    () => {
      const item = createNode({
        id: "node",
        name: "file",
        marked: false,
        stat: createFileStat({
          mode: createMode({
            owner: fullCapability(),
            group: fullCapability(),
            others: fullCapability().disallowToWrite(),
          }),
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
    },
    { decorators: [withInfo, withKnobs] }
  )
  .add(
    "marked node",
    () => {
      const item = createNode({
        id: "node",
        name: "file",
        marked: true,
        stat: createFileStat({
          mode: createMode({
            owner: fullCapability(),
            group: fullCapability(),
            others: fullCapability().disallowToWrite(),
          }),
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
    },
    { decorators: [withInfo, withKnobs] }
  );
