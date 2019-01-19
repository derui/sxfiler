import classNames from "classnames";
import * as React from "react";
import { CSSTransition } from "react-transition-group";
import { Level, Notification } from "../domain/notification";

export type TimeoutCallback = (id: string) => void;

interface Prop {
  item: Notification;
  timeouted: boolean;
  onItemTimeouted: TimeoutCallback;
}

interface State {
  mounted: boolean;
}

function assertNever(x: never): never {
  throw new Error("Unexpected object: " + x);
}

export default class NotificationItem extends React.Component<Prop, State> {
  constructor(props: Prop) {
    super(props);

    this.state = { mounted: false };
  }

  public componentDidMount() {
    this.setState({ mounted: false });
  }

  public handleItemTimeouted() {
    this.props.onItemTimeouted(this.props.item);
  }

  public render() {
    const { item } = this.props;

    const renderer = () => {
      const className = classNames("fp-NotificationList_Item", {
        "fp-NotificationList_Item-info": item.level === Level.Info,
        "fp-NotificationList_Item-warning": item.level === Level.Warning,
        "fp-NotificationList_Item-error": item.level === Level.Error,
      });

      switch (item.body.kind) {
        case "oneshot":
          return <li className={className}>{item.body.message}</li>;
        case "progress":
          return null;
        default:
          return assertNever(item.body);
      }
    };

    return (
      <CSSTransition
        in={this.state.mounted && this.props.timeouted}
        onExited={this.onItemTimeouted}
        timeout={200}
        classNames="fp-NotificationList_ItemAnimation"
      >
        {renderer}
      </CSSTransition>
    );
  }
}
