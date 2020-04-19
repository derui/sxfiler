import { h } from "preact";

import { ResizeSensor } from "@/libs/resize-sensor";
import { useState, StateUpdater } from "preact/hooks";

export type Props = {
  container?: string;
  class?: string;
  style?: object;
  children: (size: { width: number; height: number }) => preact.VNode<any>;
};

// the callback to handle resize event
function handleResize(setState: StateUpdater<{ width: number; height: number }>): (entry: ResizeObserverEntry) => void {
  return (entry) => {
    setState({
      width: entry.contentRect.width,
      height: entry.contentRect.height,
    });
  };
}

export const AutoSizer: preact.FunctionComponent<Props> = (props) => {
  const [state, setState] = useState({ width: 0, height: 0 });

  const { children, container = "div", style, ...rest } = props;
  const { width, height } = state;

  const outerStyle = { overflow: "visible" };

  let boilOnChildren = false;

  if (width === 0 && height === 0) {
    boilOnChildren = true;
  }

  return (
    <ResizeSensor onResize={handleResize(setState)}>
      {(ref) => {
        const v = h(
          container,
          {
            style: {
              ...outerStyle,
              ...style,
            },
            ...rest,
          },
          boilOnChildren ? null : children(Object.assign({}, state))
        );
        v.ref = ref;
        return v;
      }}
    </ResizeSensor>
  );
};
