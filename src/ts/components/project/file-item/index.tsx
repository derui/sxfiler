import { h } from "preact";

import { Component as Mode } from "./mode-slot";
import { Component as Name } from "./name-slot";
import { Component as Size } from "./size-slot";
import { Component as Timestamp } from "./timestamp-slot";
import { FileItem } from "@/generated/filer_pb";

export type Props = {
  item: FileItem.AsObject;
  selected: boolean;
  hidden?: boolean;
  bookmarked: boolean;
  onRefUpdated?: (e: HTMLElement) => void;
};

export const Component: preact.FunctionComponent<Props> = ({
  item,
  selected,
  bookmarked,
  hidden = false,
  onRefUpdated = undefined,
  ...rest
}) => {
  const stat = item.stat;
  if (!stat) {
    return null;
  }

  return (
    <div
      class="file-item__root"
      data-testid="fileList-item"
      aria-selected={selected}
      data-marked={item.marked}
      data-bookmarked={bookmarked}
      aria-hidden={hidden}
      ref={(e) => {
        if (onRefUpdated && e) {
          onRefUpdated(e);
        }
      }}
      {...rest}
    >
      <Mode key="mode" mode={stat.mode} isDirectory={stat.isDirectory} isSymlink={stat.isSymlink} />
      <Timestamp key="timestamp" timestamp={new Date(stat.mtime)} />
      <Size key="size" size={BigInt(stat.size)} />
      <Name key="name" name={item.name} isDirectory={stat.isDirectory} isSymlink={stat.isSymlink} />
    </div>
  );
};
Component.displayName = "FileItem";

export type ComponentType = typeof Component;
