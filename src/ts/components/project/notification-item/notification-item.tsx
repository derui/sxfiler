import * as React from "react";
import { CSSTransition } from "react-transition-group";
import { Level, MessageBody } from "../../../domains/notification";

import * as ListItem from "../../ui/list-item/list-item";

// tslint:disable-next-line
const styles: ClassNames = require("./notification-item.module.scss");

interface ClassNames {
  root: string;
  animationExit: string;
  animationExitActive: string;
  animationEnter: string;
  animationEnterActive: string;
}

export interface Props {
  body: MessageBody;
  level: Level;
  timeouted: boolean;
  onExited: () => void;
}

export class Component extends React.PureComponent<Props> {
  constructor(props: Props) {
    super(props);
  }

  public render() {
    const { body, level } = this.props;

    const renderer = () => {
      let levelStyle = "";
      switch (level) {
        case Level.Info:
          levelStyle = "info";
          break;
        case Level.Warning:
          levelStyle = "warning";
          break;
        case Level.Error:
          levelStyle = "error";
          break;
      }

      return (
        <ListItem.Component className={styles.root} data-level={levelStyle}>
          {body.message}
        </ListItem.Component>
      );
    };

    return (
      <CSSTransition
        in={!this.props.timeouted}
        onExited={this.handleExited}
        timeout={200}
        unmountOnExit={true}
        classNames={{
          enter: styles.animationEnter,
          enterActive: styles.animationEnterActive,
          exit: styles.animationExit,
          exitActive: styles.animationExitActive,
        }}
      >
        {renderer}
      </CSSTransition>
    );
  }

  private handleExited = () => {
    this.props.onExited();
  };
}
