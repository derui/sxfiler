import { h } from "preact";
import { useState, StateUpdater } from "preact/hooks";
import { Component as Switch } from "@/components/ui/switch";
import { Item, ItemElementType } from "@/configurations/types";
import { isTypedBoolean } from "@/configurations/utils";

type Callback = (value: boolean) => void;

export type Props = {
  item: Item;
  value?: boolean;
  onUpdated: Callback;
};

const handleChange = (callback: Callback, stateUpdater: StateUpdater<boolean>) => (v: boolean) => {
  stateUpdater(v);
  callback(v);
};

export const Component: preact.FunctionComponent<Props> = ({ item, onUpdated, value }) => {
  if (item.type.kind !== ItemElementType.BOOLEAN) {
    return null;
  }
  let v = item.type.defaultValue;
  if (isTypedBoolean(item, value)) {
    v = value;
  }

  const [state, updateState] = useState(v);

  return (
    <div class="configuration-cell__root" data-testid="configuration-boolean-cell-root">
      <label class="configuration-cell__label">{item.displayName}</label>
      <div class="configuration-cell__switch-container" data-testid="configuration-boolean-cell-container">
        <Switch checked={state} onChange={handleChange(onUpdated, updateState)} />
      </div>
      <p class="configuration-cell__description">{item.description}</p>
    </div>
  );
};
