import * as React from "react";

import shallowequal from "shallowequal";
import { ResizeSensor } from "../resize-sensor";

export interface Props {
  refName?: string;
  container?: string;
  className?: string;
  style?: object;
  children: (size: { width: number; height: number }) => React.ReactElement;
}

interface State {
  width: number;
  height: number;
}

// AutoSizer is HoC to add ability to handle size of component
export default class AutoSizer extends React.Component<Props, State> {
  state: State = {
    width: 0,
    height: 0,
  };

  // the callback to handle resize event
  private handleResize = (entry: ResizeObserverEntry) => {
    this.setState({
      width: entry.contentRect.width,
      height: entry.contentRect.height,
    });
  };

  public shouldComponentUpdate(newProps: Props, newState: State): boolean {
    return !shallowequal(this.props, newProps) || !shallowequal(newState, this.state);
  }

  public render(): JSX.Element {
    const { refName = "ref", children, container = "div", className, style } = this.props;
    const { width, height } = this.state;

    const outerStyle = { overflow: "visible" };

    let boilOnChildren = false;

    if (width === 0 && height === 0) {
      boilOnChildren = true;
    }

    return (
      <ResizeSensor onResize={this.handleResize} refName={refName}>
        {React.createElement(
          container,
          {
            className,
            style: {
              ...outerStyle,
              ...style,
            },
          },
          boilOnChildren ? null : children(Object.assign({}, this.state))
        )}
      </ResizeSensor>
    );
  }
}
