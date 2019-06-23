// Implement simple layouter to use render items of list to fill list based on curser on the list
import { ItemMeasureCache } from "./item-measure-cache";

/**
   A type saved item indiecs to show in a list. Each indices are zero-origin, and the range shows as `[start, stop)`
 */
export type VirtualizedWindow = {
  startIndex: number;
  stopIndex: number;
};

type InstanceProps = {
  estimatedItemSize: number;
};

type CalculationArguments = {
  cache: ItemMeasureCache;
  /**
   * The index of the cursor in the list. This value must be zero-origin
   */
  currentCursorIndex: number;
  windowHeight: number;
  listSize: number;
};

export class ListLayoutCalculator {
  private previousWindow: VirtualizedWindow | undefined;
  private instanceProps: InstanceProps = {
    estimatedItemSize: 50,
  };

  constructor(args: { estimatedItemSize?: number } = {}) {
    if (args.estimatedItemSize) {
      this.instanceProps.estimatedItemSize = args.estimatedItemSize;
    }
  }

  // get index range to show full of the body of the item in current window
  private resolveStartIndexForVisibleWindow(
    cache: ItemMeasureCache,
    listSize: number,
    windowHeight: number,
    currentCursorIndex: number
  ): number {
    let { startIndex } = this.previousWindow ? this.previousWindow : { startIndex: 0 };

    if (startIndex > currentCursorIndex) {
      return currentCursorIndex;
    } else {
      while (startIndex < currentCursorIndex) {
        let index;
        let restSize = windowHeight;
        for (index = startIndex; index < listSize; index++) {
          const measure = cache.get(index);
          let itemHeight = this.instanceProps.estimatedItemSize;

          if (measure) {
            itemHeight = measure.height;
          }
          if (restSize < itemHeight) {
            break;
          }

          restSize -= itemHeight;
        }

        if (startIndex <= currentCursorIndex && currentCursorIndex < index) {
          break;
        }

        startIndex = Math.min(startIndex + 1, listSize - 1);
      }
      return startIndex;
    }
  }

  private resolveStopIndexForStartIndex(
    cache: ItemMeasureCache,
    windowHeight: number,
    startIndex: number,
    listSize: number
  ): number {
    let restSize = windowHeight;
    for (let index = startIndex; index < listSize; index++) {
      if (restSize <= 0) {
        return index;
      }

      const measure = cache.get(index);
      let itemHeight = this.instanceProps.estimatedItemSize;

      if (measure) {
        itemHeight = measure.height;
      }
      restSize -= itemHeight;
    }

    return listSize;
  }

  // calculate layout of the list
  calculateLayout({ cache, currentCursorIndex, windowHeight, listSize }: CalculationArguments): VirtualizedWindow {
    const startIndex = this.resolveStartIndexForVisibleWindow(cache, listSize, windowHeight, currentCursorIndex);
    const stopIndex = this.resolveStopIndexForStartIndex(cache, windowHeight, startIndex, listSize);

    this.previousWindow = {
      startIndex,
      stopIndex,
    };
    return this.previousWindow;
  }

  // reset cached
  reset() {
    this.previousWindow = undefined;
  }
}
