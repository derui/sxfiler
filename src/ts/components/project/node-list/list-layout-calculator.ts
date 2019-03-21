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

  // resolve start index for new size of the list and cursor index.
  private resolveStartIndexForCursor(currentCursorIndex: number, listSize: number): number {
    if (!this.previousWindow) {
      return Math.min(currentCursorIndex, listSize);
    }

    const { startIndex: previousStartIndex, stopIndex: previousStopIndex } = this.previousWindow;
    if (previousStartIndex > currentCursorIndex) {
      return Math.min(currentCursorIndex, listSize);
    } else if (currentCursorIndex >= previousStopIndex) {
      return Math.min(previousStartIndex + (currentCursorIndex - previousStopIndex + 1), listSize);
    } else {
      return Math.min(previousStartIndex, listSize);
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
    const startIndex = this.resolveStartIndexForCursor(currentCursorIndex, listSize);
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
