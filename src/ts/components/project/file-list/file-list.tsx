import * as React from "react";
import shallowequal from "shallowequal";
import { styled } from "@/components/theme";

import { FileItem } from "@/domains/file-item";
import * as Element from "@/components/ui/element";
import * as List from "@/components/ui/list";
import * as ListItem from "@/components/project/file-item";
import { AutoSizer } from "@/libs/auto-sizer";

import { ItemMeasureCache } from "./item-measure-cache";
import { ListLayoutCalculator, VirtualizedWindow } from "./list-layout-calculator";
import { Component as Header } from "./header";

export type Props = {
  location: string;
  items: FileItem[];
  cursor: number;
  focused: boolean;
};

const Root = styled(Element.Component)`
  display: grid;
  grid-template-rows: calc(${props => props.theme.spaces.base} * 2 + 1rem) 1fr;

  margin: 0;
  padding: 0;

  color: ${props => props.theme.colors.base01};
  overflow: hidden;
  background-color: ${props => props.theme.colors.base03};

  font-size: 1rem;
`;

const AutoSizerWrapper = styled(AutoSizer)`
  width: 100%;
  height: 100%;
  margin: 0;
  padding: 0;
`;

const Empty = styled.div`
  flex: 1 1 auto;
  margin: auto 0;
  text-align: center;

  color: ${props => props.theme.colors.red};

  &:after {
    content: "no any node";
    font-family: monospace;
  }
`;

const ListRoot = styled(List.Component)`
  ${List.style}
  position: relative;

  font-size: 1rem;
  border-right: 1px solid ${props => props.theme.colors.base0};

  &:last-of-type {
    border-right: none;
  }
`;

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
      <ListRoot style={{ height: height }} key="body">
        {this.makeListItems(layout)}
      </ListRoot>
    );
  }

  private makeListItems(layout: VirtualizedWindow): JSX.Element[] {
    const { items, cursor, focused } = this.props;

    return items.slice(layout.startIndex, layout.stopIndex).map((item, index) => {
      const selected = cursor === index + layout.startIndex && focused;
      return (
        <ListItem.Component
          key={index + layout.startIndex}
          ref={e => this.itemMeasureCache.set(index + layout.startIndex, e)}
          item={item}
          selected={selected}
        />
      );
    });
  }

  public render() {
    const { items, focused } = this.props;

    return (
      <Root>
        <Header key="header" directory={this.props.location} focused={focused} />
        <AutoSizerWrapper key="sizer">
          {({ height }) => {
            return items.length === 0 ? <Empty /> : this.makeList(height);
          }}
        </AutoSizerWrapper>
      </Root>
    );
  }
}
