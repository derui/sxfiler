import * as React from "react";
import shallowequal from "shallowequal";

import { FileItem } from "../../../domains/file-item";
import * as Element from "../../ui/element/element";
import * as List from "../../ui/list/list";
import { Component as ListItem } from "../file-item/file-item";
import { Component as RootRef } from "../../ui/root-ref/root-ref";
import AutoSizer from "../../../libs/auto-sizer";

import { ItemMeasureCache } from "./item-measure-cache";
import { ListLayoutCalculator, VirtualizedWindow } from "./list-layout-calculator";

// eslint-disable-next-line @typescript-eslint/no-var-requires
const styles: ClassNames = require("./file-list.module.scss");

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
  items: FileItem[];
  cursor: number;
  focused: boolean;
}

export type ElementType = React.ReactElement<Props, React.FunctionComponent<Props>>;

export class Component extends React.Component<Props> {
  private itemMeasureCache = new ItemMeasureCache();
  private layoutCalculator: ListLayoutCalculator = new ListLayoutCalculator({
    estimatedItemSize: 24,
  });

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
      listSize: this.props.items.length,
    });

    return (
      <List.Component style={{ height: height }} className={styles.list} key="body">
        {this.makeListItems(layout)}
      </List.Component>
    );
  }

  private makeListItems(layout: VirtualizedWindow): JSX.Element[] {
    const { items, cursor, focused } = this.props;

    return items.slice(layout.startIndex, layout.stopIndex).map((item, index) => {
      const selected = cursor === index + layout.startIndex && focused;
      return (
        <RootRef key={index + layout.startIndex} rootRef={e => this.itemMeasureCache.set(index + layout.startIndex, e)}>
          <ListItem item={item} selected={selected} />
        </RootRef>
      );
    });
  }

  public render() {
    const { items, focused } = this.props;

    return (
      <Element.Component className={styles.root}>
        <Header key="header" className={styles.header} directory={this.props.location} focused={focused} />
        <AutoSizer key="sizer" className={styles.resizeContainer}>
          {({ height }) => {
            return items.length === 0 ? <div className={styles.empty} /> : this.makeList(height);
          }}
        </AutoSizer>
      </Element.Component>
    );
  }
}
