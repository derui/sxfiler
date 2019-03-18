import * as L from "./list-layout-calculator";
import { ItemMeasureCache } from "./item-measure-cache";

function createFixedSizeElement(size: { width: number; height: number }) {
  const ret = document.createElement("span");
  Object.defineProperty(ret, "clientHeight", { value: size.height });
  Object.defineProperty(ret, "clientWidth", { value: size.height });

  return ret;
}

describe("Project", () => {
  describe("Node List", () => {
    describe("List Layout Calculator", () => {
      it("calculate when no any items measured", () => {
        const instance = new L.ListLayoutCalculator({
          estimatedItemSize: 30,
        });

        const ret = instance.calculateLayout({
          cache: new ItemMeasureCache(),
          currentCursorIndex: 0,
          windowHeight: 100,
        });

        expect(ret).toEqual({
          totalHeight: 120,
          startIndex: 0,
          stopIndex: 4,
          itemLayouts: [
            { index: 0, height: 30, offset: 0 },
            { index: 1, height: 30, offset: 30 },
            { index: 2, height: 30, offset: 60 },
            { index: 3, height: 30, offset: 90 },
          ],
        });
      });

      it("calculate layout when only one item measured", () => {
        const instance = new L.ListLayoutCalculator({
          estimatedItemSize: 30,
        });

        const cache = new ItemMeasureCache();
        cache.set(0, createFixedSizeElement({ width: 10, height: 40 }));

        const ret = instance.calculateLayout({
          cache,
          currentCursorIndex: 0,
          windowHeight: 100,
        });

        expect(ret).toEqual({
          totalHeight: 100,
          startIndex: 0,
          stopIndex: 3,
          itemLayouts: [
            { index: 0, height: 40, offset: 0 },
            { index: 1, height: 30, offset: 40 },
            { index: 2, height: 30, offset: 70 },
          ],
        });
      });

      it("calculate layout when if current cursor index greater than measureable index", () => {
        const instance = new L.ListLayoutCalculator({
          estimatedItemSize: 30,
        });

        const cache = new ItemMeasureCache();
        cache.set(0, createFixedSizeElement({ width: 10, height: 60 }));

        const ret = instance.calculateLayout({
          cache,
          currentCursorIndex: 1,
          windowHeight: 100,
        });

        expect(ret).toEqual({
          totalHeight: 120,
          startIndex: 0,
          stopIndex: 3,
          itemLayouts: [
            { index: 0, height: 60, offset: 0 },
            { index: 1, height: 30, offset: 60 },
            { index: 2, height: 30, offset: 90 },
          ],
        });
      });

      it("calculate layout when item instances larger than visible area", () => {
        const instance = new L.ListLayoutCalculator({
          estimatedItemSize: 20,
        });

        const cache = new ItemMeasureCache();
        new Array(5).fill(true).forEach((_, index) => {
          cache.set(index, createFixedSizeElement({ width: 10, height: 30 }));
        });

        const ret = instance.calculateLayout({
          cache,
          currentCursorIndex: 0,
          windowHeight: 100,
        });

        expect(ret).toEqual({
          totalHeight: 120,
          startIndex: 0,
          stopIndex: 4,
          itemLayouts: [
            { index: 0, height: 30, offset: 0 },
            { index: 1, height: 30, offset: 30 },
            { index: 2, height: 30, offset: 60 },
            { index: 3, height: 30, offset: 90 },
          ],
        });
      });

      it("calculate layout when the cursor locates middle of the window", () => {
        const instance = new L.ListLayoutCalculator({
          estimatedItemSize: 20,
        });

        const cache = new ItemMeasureCache();
        new Array(5).fill(true).forEach((_, index) => {
          cache.set(index, createFixedSizeElement({ width: 10, height: 30 }));
        });

        const ret = instance.calculateLayout({
          cache,
          currentCursorIndex: 2,
          windowHeight: 100,
        });

        expect(ret).toEqual({
          totalHeight: 110,
          startIndex: 2,
          stopIndex: 6,
          itemLayouts: [
            { index: 2, height: 30, offset: 60 },
            { index: 3, height: 30, offset: 90 },
            { index: 4, height: 30, offset: 120 },
            { index: 5, height: 20, offset: 150 },
          ],
        });
      });
    });

    it("return same layout when index of the cursor contains calculated before", () => {
      const instance = new L.ListLayoutCalculator({
        estimatedItemSize: 20,
      });

      const cache = new ItemMeasureCache();
      new Array(5).fill(true).forEach((_, index) => {
        cache.set(index, createFixedSizeElement({ width: 10, height: 30 }));
      });

      const before = instance.calculateLayout({
        cache,
        currentCursorIndex: 2,
        windowHeight: 100,
      });
      const after = instance.calculateLayout({
        cache,
        currentCursorIndex: 3,
        windowHeight: 100,
        currentLayout: before,
      });

      expect(after).toEqual(before);
    });

    it("should be able to reset current information", () => {
      const instance = new L.ListLayoutCalculator({
        estimatedItemSize: 20,
      });

      const cache = new ItemMeasureCache();
      new Array(5).fill(true).forEach((_, index) => {
        cache.set(index, createFixedSizeElement({ width: 10, height: 30 }));
      });

      const before = instance.calculateLayout({
        cache,
        currentCursorIndex: 2,
        windowHeight: 100,
      });
      instance.reset();

      const after = instance.calculateLayout({
        cache,
        currentCursorIndex: 3,
        windowHeight: 100,
        currentLayout: before,
      });

      expect(after).toEqual(before);
    });
  });
});
