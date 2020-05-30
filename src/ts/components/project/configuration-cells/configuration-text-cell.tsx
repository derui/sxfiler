import { h } from "preact";
import { useState, StateUpdater } from "preact/hooks";
import { isTypedText } from "@/configurations/utils";
import { ItemElementType, Item } from "@/configurations/types";

type Callback = (value: string) => void;

export type Props = {
  item: Item;
  value?: string;
  onUpdated: Callback;
};

const handleChange = (callback: Callback, stateUpdater: StateUpdater<string>) => (e: Event) => {
  const target = e.target as HTMLInputElement;
  stateUpdater(target.value || "");
  callback(target.value || "");
};

export const Component: preact.FunctionComponent<Props> = ({ item, onUpdated, value }) => {
  if (item.type.kind !== ItemElementType.TEXT) {
    return null;
  }
  let v: string | undefined;
  if (isTypedText(item, value)) {
    v = value;
  }

  const [state, updateState] = useState(v || item.type.defaultValue || "");

  return (
    <div class="configuration-cell__root" data-testid="configuration-text-cell-root">
      <label class="configuration-cell__label">{item.displayName}</label>
      <input
        class="configuration-cell__text-input"
        data-testid="configuration-text-cell-input"
        type="text"
        value={state}
        onInput={handleChange(onUpdated, updateState)}
      ></input>
      <p class="configuration-cell__description">{item.description}</p>
    </div>
  );
};
