import * as React from "react";

import { Node } from "../../../domains/node";
import * as Element from "../../ui/element/element";
import * as List from "../../ui/list/list";
import { Component as NodeItem } from "../node-item/node-item";
import { Component as RootRef } from "../../ui/root-ref/root-ref";
import AutoSizer from "../../../libs/auto-sizer";

import { ItemMeasureCache } from "./item-measure-cache";
import { ListLayoutCalculator, Layout } from "./list-layout-calculator";

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
  needRender: boolean;
};

export class Component extends React.PureComponent<Props, State> {
  private itemMeasureCache = new ItemMeasureCache();
  private layoutCalculator: ListLayoutCalculator = new ListLayoutCalculator({
    estimatedItemSize: 24,
  });
  private currentLayout: Layout | undefined = undefined;

  state = { needRender: false };

  public componentDidMount() {
    this.setState({ needRender: true });
  }

  private makeListItems(height: number): JSX.Element[] {
    const { nodes, cursor, focused } = this.props;
    const layout = this.layoutCalculator.calculateLayout({
      cache: this.itemMeasureCache,
      currentCursorIndex: cursor,
      windowHeight: height,
      currentLayout: this.currentLayout,
    });

    return layout.itemLayouts.map(({ index }) => {
      const selected = cursor === index && focused;
      return (
        <RootRef rootRef={e => this.itemMeasureCache.set(index, e)}>
          <NodeItem key={index} item={nodes[index]} selected={selected} />
        </RootRef>
      );
    });
  }

  public render() {
    const { nodes, focused } = this.props;

    return (
      <AutoSizer className={styles.resizeContainer}>
        {({ height }) => {
          return (
            <Element.Component className={styles.root}>
              <Header key="header" className={styles.header} directory={this.props.location} focused={focused} />
              {nodes.length === 0 ? (
                <div className={styles.empty} />
              ) : (
                <List.Component className={styles.list} key="body">
                  {this.makeListItems(height)}
                </List.Component>
              )}
            </Element.Component>
          );
        }}
      </AutoSizer>
    );
  }
}
