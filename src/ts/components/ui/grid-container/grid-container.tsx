import * as React from "react";
import * as Element from "../element/element";

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

export interface Props extends ElementProps {
  direction?: DirectionProperty;
  justify?: JustifyProperty;
  alignItems?: AlignItemsProperty;
  spacing?: Spacing;
}

export const Component: React.FC<Prop> = props => {
  const {
    tagName = "div",
    direction = DirectionProperty.Row,
    justify = JustifyProperty.FlexStart,
    alignItems = AlignItemsProperty.FlexStart,
    spacing = 0,
    children,
    ...rest
  } = props;

  const style = {
    display: "grid",
    gridDirection: direction,
    justifyContent: justify,
    alignItems: alignItems,
    gridGap: `${spacing}px`,
  };

  return (<Element tagName={tagName} style={style} {...rest}>{children}</Element>)
};
