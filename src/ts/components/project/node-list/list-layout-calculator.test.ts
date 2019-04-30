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
    describe("List Window Calculator", () => {
      it("calculate start/stop index under size of the list", () => {
        const instance = new L.ListLayoutCalculator({
          estimatedItemSize: 30,
        });

        const ret = instance.calculateLayout({
          cache: new ItemMeasureCache(),
          currentCursorIndex: 0,
          windowHeight: 100,
          listSize: 2,
        });

        expect(ret).toEqual({
          startIndex: 0,
          stopIndex: 2,
        });
      });

      it("calculate when no any items measured", () => {
        const instance = new L.ListLayoutCalculator({
          estimatedItemSize: 30,
        });

        const ret = instance.calculateLayout({
          cache: new ItemMeasureCache(),
          currentCursorIndex: 0,
          windowHeight: 100,
          listSize: 5,
        });

        expect(ret).toEqual({
          startIndex: 0,
          stopIndex: 4,
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
          listSize: 5,
        });

        expect(ret).toEqual({
          startIndex: 0,
          stopIndex: 3,
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
          listSize: 100,
        });

        expect(ret).toEqual({
          startIndex: 0,
          stopIndex: 3,
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
          listSize: 100,
        });

        expect(ret).toEqual({
          startIndex: 0,
          stopIndex: 4,
        });
      });

      it("returns updated index when the cursor index is on the partially hidden item", () => {
        const instance = new L.ListLayoutCalculator({
          estimatedItemSize: 20,
        });

        const cache = new ItemMeasureCache();
        new Array(5).fill(true).forEach((_, index) => {
          cache.set(index, createFixedSizeElement({ width: 10, height: 30 }));
        });
        const args = {
          cache,
          currentCursorIndex: 0,
          windowHeight: 100,
          listSize: 100,
        };

        const initial = instance.calculateLayout(args);
        const ret = instance.calculateLayout({ ...args, currentCursorIndex: 3 });
        const ret2 = instance.calculateLayout({ ...args, currentCursorIndex: 7 });

        expect(initial).toEqual({ startIndex: 0, stopIndex: 4 });
        expect(ret).toEqual({ startIndex: 1, stopIndex: 5 });
        expect(ret2).toEqual({ startIndex: 4, stopIndex: 9 });
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
          listSize: 100,
        });

        expect(ret).toEqual({
          startIndex: 0,
          stopIndex: 4,
        });
      });

      it("should calculate based on previous window when cursor moved to next", () => {
        const instance = new L.ListLayoutCalculator({
          estimatedItemSize: 20,
        });

        const cache = new ItemMeasureCache();
        new Array(5).fill(true).forEach((_, index) => {
          cache.set(index, createFixedSizeElement({ width: 10, height: 30 }));
        });

        const firstMoved = instance.calculateLayout({
          cache,
          currentCursorIndex: 2,
          windowHeight: 100,
          listSize: 100,
        });
        const ret = instance.calculateLayout({
          cache,
          currentCursorIndex: 6,
          windowHeight: 100,
          listSize: 100,
        });

        expect(firstMoved).toEqual({
          startIndex: 0,
          stopIndex: 4,
        });
        expect(ret).toEqual({
          startIndex: 3,
          stopIndex: 7,
        });
      });

      it("should calculate based on previous window when cursor moved to previous", () => {
        const instance = new L.ListLayoutCalculator({
          estimatedItemSize: 20,
        });

        const cache = new ItemMeasureCache();
        new Array(5).fill(true).forEach((_, index) => {
          cache.set(index, createFixedSizeElement({ width: 10, height: 30 }));
        });

        instance.calculateLayout({
          cache,
          currentCursorIndex: 2,
          windowHeight: 100,
          listSize: 100,
        });
        const ret = instance.calculateLayout({
          cache,
          currentCursorIndex: 1,
          windowHeight: 100,
          listSize: 100,
        });

        expect(ret).toEqual({
          startIndex: 0,
          stopIndex: 4,
        });
      });

      it("returns difference layout when calculator resetted", () => {
        const instance = new L.ListLayoutCalculator({
          estimatedItemSize: 20,
        });

        const cache = new ItemMeasureCache();
        new Array(5).fill(true).forEach((_, index) => {
          cache.set(index, createFixedSizeElement({ width: 10, height: 30 }));
        });

        instance.calculateLayout({
          cache,
          currentCursorIndex: 2,
          windowHeight: 100,
          listSize: 100,
        });
        instance.reset();
        const ret = instance.calculateLayout({
          cache,
          currentCursorIndex: 3,
          windowHeight: 100,
          listSize: 100,
        });

        expect(ret).toEqual({
          startIndex: 1,
          stopIndex: 5,
        });
      });

      it("return window to show all items when cursor get to the last of the list", () => {
        const instance = new L.ListLayoutCalculator();

        const cache = new ItemMeasureCache();
        new Array(10).fill(true).forEach((_, index) => {
          cache.set(index, createFixedSizeElement({ width: 10, height: 30 }));
        });

        const firstMoved = instance.calculateLayout({
          cache,
          currentCursorIndex: 6,
          windowHeight: 100,
          listSize: 10,
        });
        const ret = instance.calculateLayout({
          cache,
          currentCursorIndex: 9,
          windowHeight: 100,
          listSize: 10,
        });

        expect(firstMoved).toEqual({ startIndex: 4, stopIndex: 8 });
        expect(ret).toEqual({ startIndex: 7, stopIndex: 10 });
      });
    });
  });
});
