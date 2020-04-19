import { h } from "preact";
import { useEffect, useRef, useState } from "preact/hooks";

type Handler = (newName: string) => void;

export type Props = {
  selected: boolean;
  onUpdated: Handler;
  itemName: string;
};

const handleChange = (cb: (input: string) => void) => (ev: preact.JSX.TargetedEvent<HTMLInputElement, Event>) => {
  cb(ev.currentTarget.value || "");
};

export const Component: preact.FunctionComponent<Props> = ({ selected, itemName, onUpdated }) => {
  const [state, setState] = useState(itemName);
  const refInput = useRef<HTMLInputElement>(null);

  useEffect(() => {
    if (refInput.current) {
      refInput.current.focus();
    }
  }, []);

  return (
    <div class="decision-modal__panel" data-testid="decisionModal-rename" aria-selected={selected}>
      <p class="decision-modal__label" data-testid="decisionModal-renameLabel">
        Rename
      </p>
      <label>
        <input
          class="decision-modal__input"
          data-testid="decisionModal-input"
          type="text"
          placeholder="New name of the node"
          value={state}
          onInput={handleChange((v) => {
            setState(v);
            onUpdated(v);
          })}
          ref={refInput}
        />
      </label>
    </div>
  );
};
