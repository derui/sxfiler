import * as React from "react";
import "resize-observer-polyfill";

import * as Manager from "./resize-observer";

export interface Props {
  refName?: string;
  onResize?: (entry: ResizeObserverEntry, unobserve: () => void) => void;
  children: React.ReactElement;
}

/**
   Resize Sensor provides ability to catch element resized.
 */
export class Component extends React.Component<Props> {
  private currentTarget: React.RefObject<Element> = React.createRef();

  public componentDidMount() {
    this.observe();
  }

  public componentWillUnmount() {
    this.unobserve();
  }

  public componentDidUpdate(prevProps: Props) {
    if (prevProps.onResize !== this.props.onResize) {
      this.unobserve();
      this.observe();
    }
  }

  /**
     The callback handles resize event of the element.
   */
  private handleResize = (entry: ResizeObserverEntry, unobserve: () => void) => {
    if (this.props.onResize) {
      this.props.onResize(entry, unobserve);
    }
  };

  /**
     add element to list of ResizeObserver
   */
  private observe = () => {
    if (!this.currentTarget.current) {
      return;
    }

    Manager.observe(this.currentTarget.current, this.handleResize);
  };

  /**
     remove element from list of ResizeObserver
   */
  private unobserve = () => {
    if (!this.currentTarget.current) {
      return;
    }

    Manager.unobserve(this.currentTarget.current);
  };

  public render(): JSX.Element {
    const { refName = "ref", children } = this.props;

    return React.cloneElement(children, {
      [refName]: this.currentTarget,
    });
  }
}
