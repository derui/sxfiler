import { withInfo } from "@storybook/addon-info";
import { boolean, withKnobs } from "@storybook/addon-knobs";
import { storiesOf } from "@storybook/preact";
import { h } from "preact";

import { Component as FileItemComponent } from "@/components/project/file-item";
import { also } from "@/libs/fn";
import { FileItem, FileStat, Mode } from "@/generated/filer_pb";

storiesOf("Project/File Item Item", module)
  .addParameters({ info: { inline: true } })
  .add(
    "simple file item",
    () => {
      const item = also(new FileItem(), v => {
        v.setId("file item");
        v.setName("file.txt");
        v.setMarked(false);
        v.setStat(
          also(new FileStat(), v => {
            v.setMode(new Mode());
            v.setUid(1000);
            v.setGid(1000);
            v.setAtime("0");
            v.setCtime("0");
            v.setMtime("0");
            v.setSize("10");
            v.setIsDirectory(false);
            v.setIsFile(true);
            v.setIsSymlink(false);
          })
        );
        v.setParent("/");
      });
      return (
        <div class="theme__default">
          <FileItemComponent item={item.toObject()} selected={boolean("Selected", false)} bookmarked={false} />
        </div>
      );
    },
    { decorators: [withInfo, withKnobs] }
  )
  .add(
    "simple directory file item",
    () => {
      const item = also(new FileItem(), v => {
        v.setId("file item");
        v.setName("file.txt");
        v.setMarked(false);
        v.setStat(
          also(new FileStat(), v => {
            v.setMode(new Mode());
            v.setUid(1000);
            v.setGid(1000);
            v.setAtime("0");
            v.setCtime("0");
            v.setMtime("0");
            v.setSize("10");
            v.setIsDirectory(true);
            v.setIsFile(false);
            v.setIsSymlink(false);
          })
        );
        v.setParent("/");
      });
      return (
        <div class="theme__default">
          <FileItemComponent item={item.toObject()} selected={boolean("Selected", false)} bookmarked={false} />
        </div>
      );
    },
    { decorators: [withInfo, withKnobs] }
  )
  .add(
    "simple symlink file item",
    () => {
      const item = also(new FileItem(), v => {
        v.setId("file item");
        v.setName("file.txt");
        v.setMarked(false);
        v.setStat(
          also(new FileStat(), v => {
            v.setMode(new Mode());
            v.setUid(1000);
            v.setGid(1000);
            v.setAtime("0");
            v.setCtime("0");
            v.setMtime("0");
            v.setSize("10");
            v.setIsDirectory(false);
            v.setIsFile(false);
            v.setIsSymlink(true);
          })
        );
        v.setParent("/");
      });

      return (
        <div class="theme__default">
          <FileItemComponent item={item.toObject()} selected={boolean("Selected", false)} bookmarked={true} />
        </div>
      );
    },
    { decorators: [withInfo, withKnobs] }
  )
  .add(
    "marked file item",
    () => {
      const item = also(new FileItem(), v => {
        v.setId("file item");
        v.setName("file.txt");
        v.setMarked(true);
        v.setStat(
          also(new FileStat(), v => {
            v.setMode(new Mode());
            v.setUid(1000);
            v.setGid(1000);
            v.setAtime("0");
            v.setCtime("0");
            v.setMtime("0");
            v.setSize("10");
            v.setIsDirectory(false);
            v.setIsFile(true);
            v.setIsSymlink(false);
          })
        );
        v.setParent("/");
      });

      return (
        <div class="theme__default">
          <FileItemComponent item={item.toObject()} selected={boolean("Selected", false)} bookmarked={false} />
        </div>
      );
    },
    { decorators: [withInfo, withKnobs] }
  )
  .add(
    "bookmarked file item",
    () => {
      const item = also(new FileItem(), v => {
        v.setId("file item");
        v.setName("file.txt");
        v.setMarked(false);
        v.setStat(
          also(new FileStat(), v => {
            v.setMode(new Mode());
            v.setUid(1000);
            v.setGid(1000);
            v.setAtime("0");
            v.setCtime("0");
            v.setMtime("0");
            v.setSize("10");
            v.setIsDirectory(false);
            v.setIsFile(true);
            v.setIsSymlink(false);
          })
        );
        v.setParent("/");
      });

      return (
        <div class="theme__default">
          <FileItemComponent bookmarked={true} item={item.toObject()} selected={boolean("Selected", false)} />
        </div>
      );
    },
    { decorators: [withInfo, withKnobs] }
  );
