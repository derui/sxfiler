import * as React from "react";
import shallowequal from "shallowequal";

import { NodeObject } from "../../../domains/node";
import * as Element from "../../ui/element/element";
import * as List from "../../ui/list/list";
import { Component as NodeItem } from "../node-item/node-item";
import { Component as RootRef } from "../../ui/root-ref/root-ref";
import AutoSizer from "../../../libs/auto-sizer";

import { ItemMeasureCache } from "./item-measure-cache";
import { ListLayoutCalculator, VirtualizedWindow } from "./list-layout-calculator";

// eslint-disable-next-line @typescript-eslint/no-var-requires
const styles: ClassNames = require("./node-list.module.scss");

export interface ClassNames {
  root?: string;
  resizeContainer?: string;
  header?: string;
  list?: string;
  empty?: string;
}

interface HeaderProps {
  className?: string;
  directory: string;
  focused: boolean;
}

const HeaderElement = Element.createComponent({ tagName: "header" });

/**
 * component definition for header of file list
 */
const Header: React.FunctionComponent<HeaderProps> = ({ className, directory, focused }) => {
  return (
    <HeaderElement className={className} data-focused={focused}>
      {directory}
    </HeaderElement>
  );
};

export interface Props {
  location: string;
  nodes: NodeObject[];
  cursor: number;
  focused: boolean;
}

export type ElementType = React.ReactElement<Props, React.FunctionComponent<Props>>;

export class Component extends React.Component<Props> {
  private itemMeasureCache = new ItemMeasureCache();
  private layoutCalculator: ListLayoutCalculator = new ListLayoutCalculator({
    estimatedItemSize: 24,
  });
  private layout: VirtualizedWindow | undefined;

  constructor(props: Props) {
    super(props);
  }

  public shouldComponentUpdate(newProps: Props): boolean {
    if (!shallowequal(newProps, this.props)) {
      return true;
    }

    return false;
  }

  private makeList(height: number): JSX.Element {
    const { cursor } = this.props;
    const layout = this.layoutCalculator.calculateLayout({
      cache: this.itemMeasureCache,
      currentCursorIndex: cursor,
      windowHeight: height,
      listSize: this.props.nodes.length,
    });

    return (
      <List.Component style={{ height: height }} className={styles.list} key="body">
        {this.makeListItems(layout)}
      </List.Component>
    );
  }

  private makeListItems(layout: VirtualizedWindow): JSX.Element[] {
    const { nodes, cursor, focused } = this.props;

    return nodes.slice(layout.startIndex, layout.stopIndex).map((node, index) => {
      const selected = cursor === index + layout.startIndex && focused;
      return (
        <RootRef key={index} rootRef={e => this.itemMeasureCache.set(index, e)}>
          <NodeItem item={node} selected={selected} />
        </RootRef>
      );
    });
  }

  public render() {
    const { nodes, focused } = this.props;

    return (
      <Element.Component className={styles.root}>
        <Header key="header" className={styles.header} directory={this.props.location} focused={focused} />
        <AutoSizer key="sizer" className={styles.resizeContainer}>
          {({ height }) => {
            return nodes.length === 0 ? <div className={styles.empty} /> : this.makeList(height);
          }}
        </AutoSizer>
      </Element.Component>
    );
  }
}
