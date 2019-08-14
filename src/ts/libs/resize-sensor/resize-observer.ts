/**
 * this module defines functions to manage resize observer.
 * Only one function as callback to an element, and have only one observer in application.

 Do not this module directory in application, so use ResizeSensor component in it.
 */

import ResizeObserver from "resize-observer-polyfill";

export type ResizeCallback = (entry: ResizeObserverEntry, unobserve: () => void) => void;

const observer = new ResizeObserver(observerCallback);
const callbackMap: Map<Element, ResizeCallback> = new Map();

const observerCallback = function observerCallback(entries: ResizeObserverEntry[], observer: ResizeObserver) {
  for (let entry of entries) {
    let handler = callbackMap.get(entry.target);

    if (handler) {
      handler(entry, () => observer.unobserve(entry.target));
    }
  }
};

/**
 * Add an element to observer and callback to handle the event when the element resized.
 */
export const observe = function observe(element: Element, callback: ResizeCallback) {
  observer.observe(element);
  callbackMap.set(element, callback);
};

/**
 * Remove the element from observer.
 */
export const unobserve = function unobserve(element: Element) {
  observer.unobserve(element);
  callbackMap.delete(element);
};
