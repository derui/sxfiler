import "resize-observer-polyfill";

import * as Manager from "./resize-observer";
import { useEffect, useRef } from "preact/hooks";

type OnResizeHandler = (entry: ResizeObserverEntry, unobserve: () => void) => void;

export type Props = {
  onResize?: OnResizeHandler;
  children: (ref: preact.Ref<any>) => preact.VNode<any>;
};

/**
   The callback handles resize event of the element.
*/
const handleResize = (onResize: OnResizeHandler | undefined) => (entry: ResizeObserverEntry, unobserve: () => void) => {
  if (onResize) {
    onResize(entry, unobserve);
  }
};

/**
   add element to list of ResizeObserver
*/
const observe = (currentTarget: preact.RefObject<Element>, onResize: OnResizeHandler | undefined) => {
  if (!currentTarget.current) {
    return;
  }

  Manager.observe(currentTarget.current, handleResize(onResize));
};

/**
   remove element from list of ResizeObserver
*/
const unobserve = (currentTarget: preact.RefObject<Element>) => {
  if (!currentTarget.current) {
    return;
  }

  Manager.unobserve(currentTarget.current);
};

export const Component: preact.FunctionComponent<Props> = (props) => {
  const { children } = props;
  const currentTarget = useRef<any>();

  useEffect(() => {
    observe(currentTarget, props.onResize);

    return () => unobserve(currentTarget);
  }, []);

  return children(currentTarget);
};
