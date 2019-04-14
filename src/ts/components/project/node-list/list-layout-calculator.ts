// Implement simple layouter to use render items of list to fill list based on curser on the list
import { ItemMeasureCache, Measure } from "./item-measure-cache";

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
    this.instanceProps.estimatedItemSize = args.estimatedItemSize ? args.estimatedItemSize : 50;
  }

  // get index range to show full of the body of the item in current window
  private resolveWindowForItemDoesNotHaveHiddenArea(cache: ItemMeasureCache, windowHeight: number): VirtualizedWindow {
    if (!this.previousWindow) {
      return { startIndex: -1, stopIndex: -1 };
    }

    let restSize = windowHeight;
    const { startIndex, stopIndex } = this.previousWindow;
    for (let index = startIndex; index < stopIndex; index++) {
      const measure = cache.get(index);
      let itemHeight = this.instanceProps.estimatedItemSize;

      if (measure) {
        itemHeight = measure.height;
      }
      if (restSize <= itemHeight) {
        return {
          startIndex,
          stopIndex: index,
        };
      }
      restSize -= itemHeight;
    }
    return { ...this.previousWindow };
  }

  // resolve start index for new size of the list and cursor index.
  private resolveStartIndexForCursor(
    currentCursorIndex: number,
    listSize: number,
    fullyVisibleWindow: VirtualizedWindow
  ): number {
    const maximumStartIndex = listSize - 1;
    if (!this.previousWindow) {
      return Math.min(currentCursorIndex, maximumStartIndex);
    }

    const { startIndex: previousStartIndex, stopIndex: previousStopIndex } = fullyVisibleWindow;
    if (previousStartIndex > currentCursorIndex) {
      return Math.min(currentCursorIndex, maximumStartIndex);
    } else if (currentCursorIndex >= previousStopIndex) {
      return Math.min(previousStartIndex + (currentCursorIndex - previousStopIndex + 1), maximumStartIndex);
    } else {
      return Math.min(previousStartIndex, maximumStartIndex);
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
      const measure = cache.get(index);
      let itemHeight = this.instanceProps.estimatedItemSize;

      if (measure) {
        itemHeight = measure.height;
      }
      if (restSize <= itemHeight) {
        return index + 1;
      }
      restSize -= itemHeight;
    }

    return listSize;
  }

  // calculate layout of the list
  calculateLayout({ cache, currentCursorIndex, windowHeight, listSize }: CalculationArguments): VirtualizedWindow {
    const fullyVisibleWindow = this.resolveWindowForItemDoesNotHaveHiddenArea(cache, windowHeight);
    const startIndex = this.resolveStartIndexForCursor(currentCursorIndex, listSize, fullyVisibleWindow);
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
