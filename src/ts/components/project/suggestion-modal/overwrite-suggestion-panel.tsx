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

export const Component: React.FC<Props> = ({ selected }) => {
  return (
    <Root aria-selected={selected}>
      <p>Overwrite</p>
    </Root>
  );
};
