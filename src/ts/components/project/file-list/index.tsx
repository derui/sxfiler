import { h } from "preact";

import * as ListItem from "@/components/project/file-item";
import { AutoSizer } from "@/libs/auto-sizer";

import { ItemMeasureCache } from "./item-measure-cache";
import { ListLayoutCalculator, VirtualizedWindow } from "./list-layout-calculator";
import { FileItem } from "@/generated/filer_pb";
import { Bookmark } from "@/generated/bookmark_pb";
import { useState } from "preact/hooks";

export type Props = {
  location: string;
  items: FileItem.AsObject[];
  bookmarks: Bookmark.AsObject[];
  cursor: number;
  focused: boolean;
};

function useItemMeasureCache() {
  const [cache] = useState(new ItemMeasureCache());
  return cache;
}

const useLayoutCalculator = () => {
  const [calculator] = useState(
    new ListLayoutCalculator({
      estimatedItemSize: 24,
    })
  );

  return calculator;
};

const makeList = (
  height: number,
  props: Props,
  layoutCalculator: ListLayoutCalculator,
  itemMeasureCache: ItemMeasureCache
) => {
  const layout = layoutCalculator.calculateLayout({
    cache: itemMeasureCache,
    currentCursorIndex: props.cursor,
    windowHeight: height,
    listSize: props.items.length,
  });

  return (
    <div class="file-list__list-root" data-testid="fileList-list" style={{ height: height }} key="body">
      {makeListItems(layout, props, itemMeasureCache)}
    </div>
  );
};

const makeListItems = (layout: VirtualizedWindow, props: Props, itemMeasureCache: ItemMeasureCache) => {
  let keyAsPathBookmark = props.bookmarks.reduce((accum, v) => {
    accum[v.path] = v;
    return accum;
  }, {} as { [key: string]: Bookmark.AsObject });

  return props.items.slice(layout.startIndex, layout.stopIndex).map((item, index) => {
    const selected = props.cursor === index + layout.startIndex && props.focused;

    return (
      <ListItem.Component
        key={index + layout.startIndex}
        onRefUpdated={(e: HTMLElement) => itemMeasureCache.set(index + layout.startIndex, e)}
        item={item}
        bookmarked={!!keyAsPathBookmark[item.fullPath]}
        selected={selected}
      />
    );
  });
};

export type HeaderProps = {
  directory: string;
  focused: boolean;
};

/**
 * component definition for header of file list
 */
const Header: preact.FunctionComponent<HeaderProps> = ({ directory, focused }) => {
  return (
    <header class="file-list__header" data-testid="fileList-header" data-focused={focused}>
      {directory}
    </header>
  );
};

export const Component: preact.FunctionComponent<Props> = (props) => {
  const { items, focused, location } = props;
  const cache = useItemMeasureCache();
  const calculator = useLayoutCalculator();

  return (
    <div class="file-list__root" data-testid="fileList-root">
      <Header key="header" directory={location} focused={focused} />
      <AutoSizer class="file-list__autosizer" key="sizer">
        {({ height }) => {
          return items.length === 0 ? (
            <div class="file-list__empty" data-testid="fileList-empty" />
          ) : (
            makeList(height, props, calculator, cache)
          );
        }}
      </AutoSizer>
    </div>
  );
};

export type ElementType = ReturnType<typeof Component>;
