import * as React from "react";

export interface Props {
  container: string;
  className?: string;
  style?: object;
}

interface State {
  width: number;
  height: number;
}

// AutoSizer is HoC to add ability to handle size of component
export default class AutoSizer extends React.Component<Props, State> {
  public static defaultProps: Props = {
    container: "div",
  };

  state: State = {
    width: 0,
    height: 0,
  };

  private _ref = React.createRef();

  public render(): JSX.Element {
    const { children, container, className, style } = this.props;
    const { width, height } = this.state;

    const outerStyle = { overflow: "visible" };

    let boilOnChildren = false;

    if (width === 0) {
      boilOnChildren = true;
    }

    if (height === 0) {
      boilOnChildren = true;
    }

    return React.createElement(
      container,
      {
        ref: this._ref,
        className,
        style: {
          ...outerStyle,
          ...style,
        },
      },
      boilOnChildren ? children : null
    );
  }
}
