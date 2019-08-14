import * as React from "react";
import { styled } from "@/components/theme";

import { Body } from "@/domains/progress-notification";
import * as ListItem from "@/components/ui/list-item";

const Root = styled(ListItem.Component)`
${ListItem.style}
  background-color: ${props => props.theme.colors.base02};
`;

const ProcessLabel = styled.span`
  flex: 0 0 20%;
  padding: ${props => props.theme.spaces.small};
  color: $sol_base2;

  border-right: ${props => props.theme.spaces.small} solid ${props => props.theme.colors.base3};
  max-width: 20%;

  white-space: nowrap;
  text-overflow: ellipsis;
  overflow: hidden;
  font-size: 1rem;
  font-family: monospace;
`;

const ProgressBar = styled.div`
  flex: 1 1 auto;
  height: ${props => props.theme.spaces.small};

  margin: auto ${props => props.theme.spaces.small};

  background-color: ${props => props.theme.colors.base2};

  border-radius: ${props => props.theme.baseBorderRadius};
`;

const ProgressIndicator = styled.div<{ ratio: number }>`
  height: 100%;
  background-color: ${props => props.theme.colors.blue};

  transition: width 100ms ease-out;

  border-radius: ${props => props.theme.baseBorderRadius};

  width: ${props => props.ratio}%;
`;

// make a progress bar
const makeProgressBar = function(ratio: number) {
  return (
    <ProgressBar key="progress-bar">
      <ProgressIndicator ratio={ratio} />
    </ProgressBar>
  );
};

export type Props = {
  body: Body;
};

export class Component extends React.PureComponent<Props> {
  constructor(props: Props) {
    super(props);
  }

  public render() {
    const { body } = this.props;
    const ratio = Math.min(100, (body.current / body.targeted) * 100);

    return (
      <Root>
        <ProcessLabel>{body.process}</ProcessLabel>
        {makeProgressBar(ratio)}
      </Root>
    );
  }
}
