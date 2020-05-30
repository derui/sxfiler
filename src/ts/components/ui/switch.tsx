import { h } from "preact";
import { useState, StateUpdater, useRef } from "preact/hooks";

type Callback = (value: boolean) => void;
export type Props = {
  onChange: Callback;
  checked?: boolean;
};

const handleChange = (updateState: StateUpdater<boolean>, callback: Callback) => (e: Event) => {
  const target = e.target as HTMLInputElement;
  const v = target.checked;

  updateState(v);
  callback(v);
};

export const Component: preact.FunctionalComponent<Props> = (props) => {
  const { onChange, checked = false } = props;
  const [state, updateState] = useState(checked);
  // for clicking checkbox directly
  const ref = useRef<HTMLInputElement>();

  const boxClass = `
switch__box
${state ? "switch__box--checked" : ""}
`;

  const railClass = `
switch__rail
${state ? "switch__rail--checked" : ""}
`;

  return (
    <span
      class="switch"
      data-testid="switch-root"
      onClick={() => {
        if (ref.current) {
          ref.current.click();
        }
      }}
    >
      <span class={railClass}>
        <span class={boxClass}></span>
      </span>
      <input
        ref={ref}
        class="switch__input"
        type="checkbox"
        checked={state}
        onChange={handleChange(updateState, onChange)}
      />
    </span>
  );
};
