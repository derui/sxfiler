import { withInfo } from "@storybook/addon-info";
import { boolean, number, withKnobs } from "@storybook/addon-knobs";
import { storiesOf } from "@storybook/react";
import * as React from "react";

import { createFileItem } from "@/domains/file-item";

import { Component as FileListComponent } from "@/components/project/file-list/file-list";
import { createFileStat } from "@/domains/file-stat";
import { createMode } from "@/domains/mode";
import { fullCapability, emptyCapability } from "@/domains/capability";

function makeFileItem(name: string, marked: boolean, isDirectory = false, isSymlink = false) {
  return createFileItem({
    id: "fileItem",
    name,
    marked: marked,
    stat: createFileStat({
      mode: createMode({
        owner: fullCapability().disallowToExecute(),
        group: fullCapability().disallowToExecute(),
        others: emptyCapability().allowToRead(),
      }),

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

const style = {
  height: "100px",
};

storiesOf("Project/File List", module)
  .addParameters({ info: { inline: true } })
  .add(
    "empty list",
    () => {
      return (
        <div style={style}>
          <FileListComponent
            items={[]}
            location="loc"
            cursor={number("cursor", 0)}
            focused={boolean("focused", false)}
          />
        </div>
      );
    },
    { decorators: [withInfo, withKnobs] }
  )
  .add(
    "with some items",
    () => {
      const fileItems = [
        makeFileItem("file.txt", true),
        makeFileItem("dir", false, true),
        makeFileItem("link.txt", false, false, false),
      ];
      return (
        <div style={style}>
          <FileListComponent
            items={fileItems}
            location="loc"
            cursor={number("cursor", 0)}
            focused={boolean("focused", false)}
          />
        </div>
      );
    },
    { decorators: [withInfo, withKnobs] }
  );
