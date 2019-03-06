import * as React from "react";
import { ProgressBody } from "../../../domains/notification";
import * as Element from "../../ui/element/element";
import * as ListItem from "../../ui/list-item/list-item";

// eslint-disable-next-line @typescript-eslint/no-var-requires
const styles = require("./progress-notification-item.module.scss");

// make a progress bar
function makeProgressBar(ratio: number) {
  const style = {
    width: `${ratio}%`,
  };

  return (
    <div key="progress-bar" className={styles.progressBar}>
      <div className={styles.progressIndicator} style={style} />
    </div>
  );
}

const Span = Element.createComponent({ tagName: "span" });

export interface Props {
  body: ProgressBody;
}

export class Component extends React.PureComponent<Props> {
  constructor(props: Props) {
    super(props);
  }

  public render() {
    const { body } = this.props;
    const ratio = Math.min(100, (body.current / body.target) * 100);

    return (
      <ListItem.Component className={styles.root}>
        <Span className={styles.processLabel}>{body.process}</Span>
        {makeProgressBar(ratio)}
      </ListItem.Component>
    );
  }
}
