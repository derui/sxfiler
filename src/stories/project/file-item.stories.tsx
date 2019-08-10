import { withInfo } from "@storybook/addon-info";
import { boolean, withKnobs } from "@storybook/addon-knobs";
import { storiesOf } from "@storybook/react";
import * as React from "react";
import { Theme, ThemeProvider } from "@/components/theme";

import { createFileItem } from "@/domains/file-item";

import { Component as FileItemComponent } from "@/components/project/file-item/file-item";
import { createFileStat } from "@/domains/file-stat";
import { emptyMode, createMode } from "@/domains/mode";
import { fullCapability, disallowToWrite } from "@/domains/capability";

storiesOf("Project/File Item Item", module)
  .addParameters({ info: { inline: true } })
  .add(
    "simple file file item",
    () => {
      const item = createFileItem({
        id: "file item",
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
      return (
        <ThemeProvider theme={Theme}>
          <FileItemComponent item={item} selected={boolean("Selected", false)} />
        </ThemeProvider>
      );
    },
    { decorators: [withInfo, withKnobs] }
  )
  .add(
    "simple directory file item",
    () => {
      const item = createFileItem({
        id: "file item",
        name: "file",
        marked: false,
        stat: createFileStat({
          mode: createMode({
            owner: fullCapability(),
            group: fullCapability(),
            others: disallowToWrite(fullCapability()),
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
      return (
        <ThemeProvider theme={Theme}>
          <FileItemComponent item={item} selected={boolean("Selected", false)} />
        </ThemeProvider>
      );
    },
    { decorators: [withInfo, withKnobs] }
  )
  .add(
    "simple symlink file item",
    () => {
      const item = createFileItem({
        id: "file item",
        name: "file",
        marked: false,
        stat: createFileStat({
          mode: createMode({
            owner: fullCapability(),
            group: fullCapability(),
            others: disallowToWrite(fullCapability()),
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
      return (
        <ThemeProvider theme={Theme}>
          <FileItemComponent item={item} selected={boolean("Selected", false)} />
        </ThemeProvider>
      );
    },
    { decorators: [withInfo, withKnobs] }
  )
  .add(
    "marked file item",
    () => {
      const item = createFileItem({
        id: "file item",
        name: "file",
        marked: true,
        stat: createFileStat({
          mode: createMode({
            owner: fullCapability(),
            group: fullCapability(),
            others: disallowToWrite(fullCapability()),
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
      return (
        <ThemeProvider theme={Theme}>
          <FileItemComponent item={item} selected={boolean("Selected", false)} />
        </ThemeProvider>
      );
    },
    { decorators: [withInfo, withKnobs] }
  );
