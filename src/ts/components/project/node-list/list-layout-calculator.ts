import { ItemMeasureCache, Measure } from "./item-measure-cache";
import { unwrap } from "../../../utils";

export type ItemLayout = {
  index: number;
  height: number;
  offset: number;
};

type ItemMetadata = {
  offset: number;
  size: Measure;
};

export type Layout = {
  // total height of layout
  totalHeight: number;
  startIndex: number;
  stopIndex: number;
  itemLayouts: Array<ItemLayout>;
};

type InstanceProps = {
  itemMetadataMap: Map<number, ItemMetadata>;
  lastMeasuredIndex: number;
  estimatedItemSize: number;
};

// get and cache item metadata.
// This function do not handle item that have not measured, so the size of it is estimated.
function getItemMetadata(
  targetIndex: number,
  instanceProps: InstanceProps,
  getItemSize: (index: number) => Measure | undefined
): ItemMetadata {
  const { lastMeasuredIndex, itemMetadataMap } = instanceProps;
  if (targetIndex < 0) {
    throw Error(`index must be greater equal than 0: ${targetIndex}`);
  }

  if (targetIndex > lastMeasuredIndex) {
    let lastMetadata =
      lastMeasuredIndex < 0
        ? {
            offset: 0,
            size: {
              width: 0,
              height: 0,
            },
          }
        : unwrap(itemMetadataMap.get(lastMeasuredIndex));
    let offset = lastMetadata.offset + lastMetadata.size.height;

    for (let index = lastMeasuredIndex + 1; index <= targetIndex; index++) {
      let size = getItemSize(index);

      // If item have not measured, can not count last measured index
      if (!size) {
        itemMetadataMap.set(index, {
          offset,
          size: {
            width: instanceProps.estimatedItemSize,
            height: instanceProps.estimatedItemSize,
          },
        });
        offset = offset + instanceProps.estimatedItemSize;
      } else {
        itemMetadataMap.set(index, { offset, size });
        offset = offset + size.height;

        instanceProps.lastMeasuredIndex =
          instanceProps.lastMeasuredIndex === index - 1 ? index : instanceProps.lastMeasuredIndex;
      }
    }
  }
  return unwrap(itemMetadataMap.get(targetIndex));
}

// find index of the item having nearest offset given it.
function findStartIndexForOffset(offset: number, instanceProps: InstanceProps): number {
  const { lastMeasuredIndex, itemMetadataMap } = instanceProps;

  let high = lastMeasuredIndex;
  let low = 0;
  let searchIndex = 0;
  while (low < high) {
    searchIndex = Math.floor((high - low) / 2) + low;
    let itemOffset = unwrap(itemMetadataMap.get(searchIndex)).offset;

    if (itemOffset < offset) {
      low = searchIndex;
    } else if (itemOffset > offset) {
      high = searchIndex;
    } else {
      break;
    }
  }

  return searchIndex;
}

function findStopIndexForStartIndex(windowHeight: number, startIndex: number, instanceProps: InstanceProps): number {
  let height = 0;
  let index = startIndex;

  while (height < windowHeight) {
    let metadata = instanceProps.itemMetadataMap.get(index);

    if (metadata) {
      height += metadata.size.height;
    } else {
      height += instanceProps.estimatedItemSize;
    }

    index++;
  }

  return index;
}

type CalculationArguments = {
  cache: ItemMeasureCache;
  currentCursorIndex: number;
  windowHeight: number;
  currentLayout?: Layout;
};

export class ListLayoutCalculator {
  private instanceProps: InstanceProps = {
    itemMetadataMap: new Map(),
    lastMeasuredIndex: -1,
    estimatedItemSize: 50,
  };

  constructor(args: { estimatedItemSize?: number } = {}) {
    this.instanceProps.estimatedItemSize = args.estimatedItemSize ? args.estimatedItemSize : 50;
  }

  private getItemSize(cache: ItemMeasureCache, index: number) {
    const size = cache.get(index);

    if (size) {
      return size;
    }
  }

  // get offset based of current cursor, and preload metadatas to fill list window
  private calculateOffsetFromCurrentCursor(cache: ItemMeasureCache, windowHeight: number, currentCursorIndex: number) {
    const getItemSize = (index: number) => this.getItemSize(cache, index);
    let metadataForCurrentCursor = getItemMetadata(currentCursorIndex, this.instanceProps, getItemSize);
    let offset = metadataForCurrentCursor.offset;

    // load item size filling window if it exists
    let height = 0;
    let index = currentCursorIndex;
    while (height < windowHeight) {
      let metadata = getItemMetadata(index, this.instanceProps, getItemSize);
      height += metadata.size.height;
      index++;
    }

    return offset;
  }

  // get item layouts in list
  private resolveItemLayouts(startIndex: number, stopIndex: number): Array<ItemLayout> {
    const ret = [];
    for (let index = startIndex; index < stopIndex; index++) {
      let metadata = this.instanceProps.itemMetadataMap.get(index);

      if (metadata) {
        ret.push({
          index,
          height: metadata.size.height,
          offset: metadata.offset,
        });
      } else if (ret.length > 0) {
        ret.push({
          index,
          height: this.instanceProps.estimatedItemSize,
          offset: 0,
        });
      }
    }

    return ret;
  }

  // calculate layout of the list
  calculateLayout({ cache, currentCursorIndex, windowHeight, currentLayout }: CalculationArguments): Layout {
    // when current layout that calculated before passed, and index of the cursor contains
    // start/stop, no need calculation
    if (
      currentLayout &&
      currentLayout.startIndex <= currentCursorIndex &&
      currentCursorIndex < currentLayout.stopIndex
    ) {
      return { ...currentLayout };
    }

    const targetOffset = this.calculateOffsetFromCurrentCursor(cache, windowHeight, currentCursorIndex);
    const startIndex = findStartIndexForOffset(targetOffset, this.instanceProps);
    const stopIndex = findStopIndexForStartIndex(windowHeight, startIndex, this.instanceProps);
    const itemLayouts = this.resolveItemLayouts(startIndex, stopIndex);
    const totalHeight = itemLayouts.reduce((accum, itemLayout) => accum + itemLayout.height, 0);

    return {
      totalHeight,
      startIndex,
      stopIndex,
      itemLayouts,
    };
  }

  // reset cached
  reset() {
    this.instanceProps = { ...this.instanceProps, itemMetadataMap: new Map(), lastMeasuredIndex: -1 };
  }
}
