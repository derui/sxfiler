import * as React from "react";
import { styled } from "@/components/theme";

import * as Domain from "@/domains/file-item";
import * as ListItem from "@/components/ui/list-item";
import { Component as Mode } from "./mode-slot";
import { Component as Name } from "./name-slot";
import { Component as Size } from "./size-slot";
import { Component as Timestamp } from "./timestamp-slot";
import { sizeAsBigInt } from "@/domains/file-stat";

export type Props = ListItem.Props & {
  item: Domain.FileItem;
  selected: boolean;
  hidden?: boolean;
  bookmarked: boolean;
};

const Element = styled(ListItem.Component)`
${ListItem.style}
  padding: ${props => props.theme.spaces.small} 0;
  width: 100%;

  background-color: ${props => props.theme.colors.base03};

  font-family: monospace;
  color: ${props => props.theme.colors.base2};
  white-space: nowrap;

  font-size: 1rem;
  border-left: 4px solid transparent;

  &[aria-selected="true"] {
    border-left: 4px solid ${props => props.theme.colors.orange};
  }

  &[aria-hidden="true"] {
    visibility: hidden;
  }

  &[data-marked="true"] {
    background-color: ${props => props.theme.colors.blue}3d;
  }

&[data-bookmarked="true"]{
    background-color: ${props => props.theme.colors.cyan}3d;
}
`;

const render = function renderFileItem(
  { item, selected, bookmarked, hidden = false, ...rest }: Props,
  ref: React.Ref<HTMLElement>
) {
  return (
    <Element
      selected={selected}
      data-marked={item.marked}
      data-bookmarked={bookmarked}
      aria-hidden={hidden}
      ref={ref}
      {...rest}
    >
      <Mode key="mode" mode={item.stat.mode} isDirectory={item.stat.isDirectory} isSymlink={item.stat.isSymlink} />
      <Timestamp key="timestamp" timestamp={item.stat.mtime} />
      <Size key="size" size={sizeAsBigInt(item.stat)} />
      <Name key="name" name={item.name} isDirectory={item.stat.isDirectory} isSymlink={item.stat.isSymlink} />
    </Element>
  );
};
render.displayName = "FileItem";

export const Component = React.forwardRef(render);

export type ComponentType = React.ComponentType<Props & React.RefAttributes<HTMLElement>>;
