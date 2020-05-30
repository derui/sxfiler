import { h } from "preact";
import { Item, ItemElementType } from "@/configurations/types";
import { Validators } from "@/validations";
import { useState, StateUpdater } from "preact/hooks";
import { isTypedNumber } from "@/configurations/utils";

type Callback = (value: string) => void;

export type Props = {
  item: Item;
  value?: number;
  onUpdated: Callback;
};

const handleChange = (callback: Callback, stateUpdater: StateUpdater<string>) => (e: Event) => {
  const target = e.target as HTMLInputElement;

  if (!Validators.validateInteger(target.value)) {
    return;
  }

  stateUpdater(target.value);
  callback(target.value || "");
};

export const Component: preact.FunctionComponent<Props> = ({ item, onUpdated, value }) => {
  if (item.type.kind !== ItemElementType.NUMBER) {
    return null;
  }
  let v: number | undefined;
  if (isTypedNumber(item, value)) {
    v = value;
  }

  const [state, updateState] = useState(Number(v || item.type.defaultValue).toString());

  return (
    <div class="configuration-cell__root" data-testid="configuration-number-cell-root">
      <label class="configuration-cell__label">{item.displayName}</label>
      <input
        class="configuration-cell__number-input"
        data-testid="configuration-number-cell-input"
        type="number"
        value={state}
        onInput={handleChange(onUpdated, updateState)}
      ></input>
      <p class="configuration-cell__description">{item.description}</p>
    </div>
  );
};
