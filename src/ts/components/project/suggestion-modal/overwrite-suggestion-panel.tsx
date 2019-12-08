import * as React from "react";

import { styled } from "@/components/theme";
import * as Element from "@/components/ui/element";
import * as Panel from "./panel";

export type Props = {
  selected: boolean;
};

const Root = styled(Element.Component)`
  ${Panel.rootStyle}
`;

const Label = styled.p`
  margin: 0px;
`;

export const Component: React.FC<Props> = ({ selected }) => {
  return (
    <Root aria-selected={selected}>
      <Label>Overwrite</Label>
    </Root>
  );
};
