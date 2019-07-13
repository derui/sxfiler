import * as React from "react";
import { styled } from "@/components/theme";

interface Prop {
  isDirectory: boolean;
  isSymlink: boolean;
  name: string;
}

const Name = styled.span`
  flex: 1 0 auto;
  padding: 0;
  font-weight: bold;

  &[data-directory="true"] {
    color: ${props => props.theme.colors.yellow};
  }

  &[data-symlink="true"] {
    color: ${props => props.theme.colors.orange};
  }
`;

export const Component: React.FC<Prop> = ({ isDirectory, name, isSymlink }) => (
  <Name data-directory={isDirectory && !isSymlink} data-symlink={isSymlink}>
    {name}
  </Name>
);
