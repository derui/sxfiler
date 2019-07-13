import * as React from "react";
import { styled } from "@/components/theme";
import * as Element from "@/components/ui/element";
import * as Panel from "./panel";
import { ReplyPayload, createRenamePayload } from "@/domains/task-reply";

type Handler = (payload: ReplyPayload) => void;

export type Props = {
  selected: boolean;
  onUpdated: Handler;
  nodeName: string;
};

const Root = styled(Element.Component)`
  ${Panel.rootStyle}
`;

const Input = styled.input`
  padding: ${props => props.theme.spaces.small};
`;

const handleChange = (handler: Handler) => (ev: React.ChangeEvent) =>
  handler(createRenamePayload(ev.target.nodeValue || ""));

export const Component: React.FC<Props> = ({ selected, nodeName, onUpdated }) => {
  return (
    <Root aria-selected={selected}>
      <p>Rename</p>
      <label>
        <Input type="text" value={nodeName} onChange={handleChange(onUpdated)} />
      </label>
    </Root>
  );
};
