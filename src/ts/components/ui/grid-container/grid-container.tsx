import classnames from "classnames";
import * as React from "react";
import * as Element from "../element/element";
import { JustifyContentProperty } from "csstype";

// tslint:disable-next-line
const defaultClassNames: ClassNames = require("./grid-container.module.scss");

export type ClassNames = {
  // root class name
  container?: string;

  // class names for align items
  alignItemsFlexStart?: string;
  alignItemsCenter?: string;
  alignItemsFlexEnd?: string;
  alignItemsStretch?: string;
  alignItemsBaseline?: string;

  // class names for justify property
  justifyFlexStart?: string;
  justifyCenter?: string;
  justifyFlexEnd?: string;
  justifySpaceBetween?: string;
  justifySpaceAround?: string;
  justifySpaceEvenly?: string;

  // class names for direction
  directionRow?: string;
  directionColumn?: string;

  // class names for spacing
  spacing0?: string;
  spacing8?: string;
  spacing16?: string;
  spacing24?: string;
  spacing32?: string;
  spacing40?: string;
}

export enum AlignItemsProperty {
  FlexStart = "flex-start",
  Center = "center",
  FlexEnd = "flex-end",
  Stretch = "stretch",
  Baseline = "baseline",
}

export enum JustifyProperty {
  FlexStart = "flex-start",
  Center = "center",
  FlexEnd = "flex-end",
  SpaceBetween = "space-between",
  SpaceAround = "space-around",
  SpaceEvenly = "space-evenly",
}

export enum DirectionProperty {
  Row = "row",
  Column = "column",
}

type Spacing = 0 | 8 | 16 | 24 | 32 | 40;

export type Props = Element.Props & {
  classNames?: ClassNames;
  tagName?: string;
  direction?: DirectionProperty;
  justify?: JustifyProperty;
  alignItems?: AlignItemsProperty;
  spacing?: Spacing;
}

// choice class name for justify-content from ClassNames
function choiceClassNameForJustify(justify: JustifyContentProperty, classNames: ClassNames) {
  switch (justify) {
    case JustifyProperty.Center:
      return classNames.justifyCenter;
    case JustifyProperty.FlexEnd:
      return classNames.justifyFlexEnd;
    case JustifyProperty.FlexStart:
      return classNames.justifyFlexStart;
    case JustifyProperty.SpaceAround:
      return classNames.justifySpaceAround;
    case JustifyProperty.SpaceBetween:
      return classNames.justifySpaceBetween;
    case JustifyProperty.SpaceEvenly:
      return classNames.justifySpaceEvenly;
    default:
      return undefined;
  }
}

// choice class name for align-items from ClassNames
function choiceClassNameForAlignItems(alignItems: AlignItemsProperty, classNames: ClassNames) {
  switch (alignItems) {
    case AlignItemsProperty.Center:
      return classNames.alignItemsCenter;
    case AlignItemsProperty.FlexEnd:
      return classNames.justifyFlexEnd;
    case AlignItemsProperty.FlexStart:
      return classNames.justifyFlexStart;
    case AlignItemsProperty.Baseline:
      return classNames.justifySpaceAround;
    case AlignItemsProperty.Stretch:
      return classNames.justifySpaceBetween;
    default:
      return undefined;
  }
}

// choice class name for spacing from ClassNames
function choiceClassNameForSpacing(spacing: Spacing, classNames: ClassNames) {
  switch (spacing) {
    case 0:
      return classNames.spacing0;
    case 8:
      return classNames.spacing8
    case 16:
      return classNames.spacing16;
    case 24:
      return classNames.spacing24;
    case 32:
      return classNames.spacing32;
    case 40:
      return classNames.spacing40;
    default:
      return undefined;
  }
}

// choice class name for direction from ClassNames
function choiceClassNameForDirection(direction: DirectionProperty, classNames: ClassNames) {
  switch (direction) {
    case DirectionProperty.Row:
      return classNames.directionRow;
    case DirectionProperty.Column:
      return classNames.directionColumn;
    default:
      return undefined;
  }
}

export const Component: React.FC<Props> = ({
  className,
  classNames = defaultClassNames,
  tagName = "div",
  direction = DirectionProperty.Row,
  justify = JustifyProperty.FlexStart,
  alignItems = AlignItemsProperty.FlexStart,
  spacing = 0,
  children,
  ...props
}) => {
  const style = {
    display: "grid",
    gridDirection: direction,
    justifyContent: justify,
    alignItems: alignItems,
    gridGap: `${spacing}px`,
  };
  const Container = Element.createComponent({ tagName });
  const joinedClassName = classnames(classNames.container,
    choiceClassNameForJustify(justify, classNames),
    choiceClassNameForAlignItems(alignItems, classNames),
    choiceClassNameForSpacing(spacing, classNames),
    choiceClassNameForDirection(direction, classNames),
  );

  return (<Container className={className || joinedClassName} {...props}>{children}</Container>)
};
