import * as React from "react";
import shallowequal from "shallowequal";

import { Node } from "../../../domains/node";
import * as Element from "../../ui/element/element";
import * as List from "../../ui/list/list";
import { Component as NodeItem } from "../node-item/node-item";
import { Component as RootRef } from "../../ui/root-ref/root-ref";
import AutoSizer from "../../../libs/auto-sizer";

import { ItemMeasureCache, Measure } from "./item-measure-cache";
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
  nodes: Node[];
  cursor: number;
  focused: boolean;
}

export type ElementType = React.ReactElement<Props, React.FunctionComponent<Props>>;

type State = {
  layout: VirtualizedWindow | undefined;
  cache: ReadonlyMap<number, Measure>;
};

export class Component extends React.Component<Props, State> {
  private itemMeasureCache = new ItemMeasureCache();
  private layoutCalculator: ListLayoutCalculator = new ListLayoutCalculator({
    estimatedItemSize: 24,
  });
  private unobserve: (() => void) | undefined = undefined;

  constructor(props: Props) {
    super(props);

    this.state = {
      layout: undefined,
      cache: new Map(),
    };

    this.unobserve = this.itemMeasureCache.observe(this.handleChangeCache);
  }

  private handleChangeCache = (cache: ReadonlyMap<number, Measure>) => {
    this.setState({ cache });
  };

  public shouldComponentUpdate(newProps: Props, newState: State): boolean {
    if (!shallowequal(newProps, this.props)) {
      return true;
    }

    if (!shallowequal(this.state.cache, newState.cache) || shallowequal(this.state.layout, newState.layout)) {
      return true;
    }

    return false;
  }

  public componentWillUnmount() {
    if (this.unobserve) {
      this.unobserve();
    }
  }

  private makeList(height: number): JSX.Element {
    const { cursor } = this.props;
    const layout = this.layoutCalculator.calculateLayout({
      cache: this.itemMeasureCache,
      currentCursorIndex: cursor,
      windowHeight: height,
      listSize: this.props.nodes.length,
    });

    console.log(layout);
    this.setState({ layout });

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
