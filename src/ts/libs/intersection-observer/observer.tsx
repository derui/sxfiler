import * as React from "react";
import * as observer from "./observer-manager";

export interface Props {
  rootId: string;
  onChange?: (entry: IntersectionObserverEntry, unobserve: () => void) => void;
}

export class Component extends React.Component<Props> {
  private currentTarget: observer.IntersectionTarget | null = null;

  componentDidMount() {
    this.observe();
  }

  // unobserve when unmount
  componentWillUnmount() {
    this.unobserve();
  }

  componentDidUpdate(prevProps: Props) {
    if (this.currentTarget) {
      let changed = false;
      if (prevProps.rootId !== this.props.rootId || prevProps.onChange !== this.props.onChange) {
        this.unobserve();
        changed = true;
      }

      if (changed) {
        this.observe();
      }
    }
  }

  observe = () => {
    if (!this.currentTarget) {
      return;
    }

    observer.observe(this.props.rootId, this.currentTarget);
  };

  unobserve = () => {
    if (this.currentTarget) {
      observer.unobserve(this.currentTarget);
    }
  };

  // call handler if changed the state of intersection
  handleChange = (entry: IntersectionObserverEntry) => {
    if (this.props.onChange) {
      this.props.onChange(entry, this.unobserve);
    }
  };

  // observing new element
  handleNode = (element: Element | null) => {
    if (this.currentTarget) {
      this.unobserve();
    }

    if (!element) {
      return;
    }

    this.currentTarget = {
      element,
      handler: this.handleChange,
    };
  };

  render() {
    const child = React.Children.only(this.props.children);
    if (!React.isValidElement(child)) {
      return null;
    }

    return React.cloneElement(child as React.ReactElement<any>, {
      ref: this.handleNode,
    });
  }
}
