import { withInfo } from "@storybook/addon-info";
import { boolean, number, withKnobs } from "@storybook/addon-knobs";
import { storiesOf } from "@storybook/preact";
import { h } from "preact";

import { Component as FileListComponent } from "@/components/project/file-list";
import { also } from "@/libs/fn";
import { FileItem, FileStat, Mode } from "@/generated/filer_pb";

function makeFileItem(name: string, marked: boolean, isDirectory = false, isSymlink = false) {
  return also(new FileItem(), v => {
    v.setId(name);
    v.setName(name);
    v.setMarked(marked);
    v.setStat(
      also(new FileStat(), v => {
        v.setMode(new Mode());
        v.setUid(1000);
        v.setGid(1000);
        v.setAtime("0");
        v.setCtime("0");
        v.setMtime("0");
        v.setSize("10");
        v.setIsDirectory(isDirectory);
        v.setIsFile(!isDirectory && !isSymlink);
        v.setIsSymlink(isSymlink);
      })
    );
    v.setParent("/");
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
        <div class="theme__default" style={style}>
          <FileListComponent
            items={[]}
            location="loc"
            bookmarks={[]}
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
      ].map(v => v.toObject());
      return (
        <div class="theme__default" style={style}>
          <FileListComponent
            items={fileItems}
            bookmarks={[]}
            location="loc"
            cursor={number("cursor", 0)}
            focused={boolean("focused", false)}
          />
        </div>
      );
    },
    { decorators: [withInfo, withKnobs] }
  );
