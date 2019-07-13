import * as React from "react";
import { styled } from "@/components/theme";

import * as Domain from "@/domains/file-item";
import * as ListItem from "@/components/ui/list-item";
import { Component as Mode } from "./mode-slot";
import { Component as Name } from "./name-slot";
import { Component as Size } from "./size-slot";
import { Component as Timestamp } from "./timestamp-slot";
import { ForwardedRef } from "@/components/ui/util";

export type Props = ForwardedRef & {
  item: Domain.FileItem;
  selected: boolean;
  hidden?: boolean;
};

const Element = styled(ListItem.Component)`
${ListItem.style}
  padding: ${props => props.theme.spaces.base} 0;
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

  &[aria-hidder="true"] {
    visibility: hidden;
  }

  &[data-marked="true"] {
    background-color: ${props => props.theme.colors.blue}3d;
  }
`;

export const Component: React.FC<Props> = ({ item, selected, hidden = false, ...rest }) => {
  return (
    <Element aria-selected={selected} data-marked={item.marked} aria-hidden={hidden} {...rest}>
      <Mode key="mode" mode={item.stat.mode} isDirectory={item.stat.isDirectory} isSymlink={item.stat.isSymlink} />
      <Timestamp key="timestamp" timestamp={item.stat.mtime} />
      <Size key="size" size={item.stat.sizeAsBigInt()} />
      <Name key="name" name={item.name} isDirectory={item.stat.isDirectory} isSymlink={item.stat.isSymlink} />
    </Element>
  );
};
