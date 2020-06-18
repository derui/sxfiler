import { h } from "preact";
import { Item, SelectOption, ItemElementType } from "@/configurations/types";

type Callback = (value: string) => void;

export type Props = {
  item: Item;
  onUpdated: Callback;
};

const handleChange = (callback: Callback) => (e: Event) => {
  const target = e.target as HTMLInputElement;
  callback(target.value);
};

const makeOption = (defaultValue: string) => (option: SelectOption, index: number) => {
  return (
    <option
      class="configuration-cell__select-option"
      key={option.value}
      data-testid={`configuration-select-option-${index}`}
      value={option.value}
      selected={defaultValue === option.value}
    >
      {option.display}
    </option>
  );
};

export const Component: preact.FunctionComponent<Props> = ({ item, onUpdated }) => {
  if (item.type.kind !== ItemElementType.SELECT_ONE) {
    return null;
  }

  return (
    <div class="configuration-cell__root" data-testid="configuration-select-one-cell-root">
      <label class="configuration-cell__label">{item.displayName}</label>
      <div class="configuration-cell__select-container" data-testid="configuration-select-one-cell-container">
        <select
          class="configuration-cell__select-input"
          data-testid="configuration-select"
          name={item.key}
          onChange={handleChange(onUpdated)}
        >
          {item.type.options.map(makeOption(item.type.defaultValue))}
        </select>
      </div>
      <p class="configuration-cell__description">{item.description}</p>
    </div>
  );
};
