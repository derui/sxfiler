import * as React from "react";
import { styled } from "@/components/theme";

export type Props = {
  start: number;
  windowSize: number;
};

const Root = styled.div`
  position: absolute;
  right: 0;
  bottom: 0;
  top: 0;

  width: ${props => props.theme.spaces.small};
  height: 100%;

  background-color: transparent;

  padding: ${props => props.theme.spaces.base};
`;

const Bar = styled.div<{ top: number; height: number }>`
  position: relative;
  display: inline-block;

  border-radius: ${props => props.theme.spaces.small};
  background-color: ${props => props.theme.colors.base01};

  top: ${props => props.top}%;
  height: ${props => props.height}%;
`;

export const Component: React.FC<Props> = ({ start, windowSize }) => {
  const top = start * 100.0;
  const height = windowSize * 100.0;

  return (
    <Root>
      <Bar top={top} height={height} />
    </Root>
  );
};
