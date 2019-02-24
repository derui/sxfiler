import classNames from "classnames";
import * as React from "react";

// tslint:disable-next-line
const styles = require("./grid-container.test.module.scss");

enum AlignItemsProperty {
  FlexStart = "flex-start",
  Center = "center",
  FlexEnd = "flex-end",
  Stretch = "stretch",
  Baseline = "baseline",
}

enum JustifyProperty {
  FlexStart = "flex-start",
  Center = "center",
  FlexEnd = "flex-end",
  SpaceBetween = "space-between",
  SpaceAround = "space-around",
  SpaceEvenly = "space-evenly",
}

enum DirectionProperty {
  Row = "row",
  RowReverse = "row-reverse",
  Column = "column",
  ColumnReverse = "column-reverse",
}

type Spacing = 0 | 8 | 16 | 24 | 32 | 40;

interface Prop extends React.HTMLAttributes<HTMLElement> {
  classes?: string[];
  component?: string;
  direction?: DirectionProperty;
  justify?: JustifyProperty;
  alignItems?: AlignItemsProperty;
  spacing?: Spacing;
}

const FileList: React.FC<Prop> = props => {
  const {
    classes,
    component = "div",
    direction = DirectionProperty.Row,
    justify = JustifyProperty.FlexStart,
    alignItems = AlignItemsProperty.FlexStart,
    spacing = 0,
    children, ...rest
  } = props;
  const className = classes ? classNames(classes) : styles.list;

  return React.createElement(component, { ...rest, className }, children);
};

export default FileList;
