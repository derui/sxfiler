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

const handleChange = (cb: (input: string) => void, handler: Handler) => (ev: React.ChangeEvent<HTMLInputElement>) => {
  cb(ev.target.value || "");
  handler(createRenamePayload(ev.target.value || ""));
};

export const Component: React.FC<Props> = ({ selected, nodeName, onUpdated }) => {
  const [state, setState] = React.useState(nodeName);
  const refInput = React.useRef<HTMLInputElement>(null);

  React.useEffect(() => {
    if (refInput.current) {
      refInput.current.focus();
    }
  }, []);

  return (
    <Root aria-selected={selected}>
      <p>Rename</p>
      <label>
        <Input type="text" value={state} onChange={handleChange(setState, onUpdated)} ref={refInput} />
      </label>
    </Root>
  );
};
