import * as React from "react";
import { styled } from "@/components/theme";

export type HeaderProps = {
  directory: string;
  focused: boolean;
};

const HeaderElement = styled.header`
  flex: 0 0 auto;
  padding: ${props => props.theme.spaces.base} 0;
  margin: 0;

  font-weight: bold;
  color: ${props => props.theme.colors.base1};
  background-color: ${props => props.theme.colors.base03};
  font-family: monospace;

  border-bottom: 1px solid ${props => props.theme.colors.base01};

  &[data-focused="true"] {
    color: ${props => props.theme.colors.yellow};
  }
`;

/**
 * component definition for header of file list
 */
export const Component: React.FunctionComponent<HeaderProps> = ({ directory, focused }) => {
  return <HeaderElement data-focused={focused}>{directory}</HeaderElement>;
};
