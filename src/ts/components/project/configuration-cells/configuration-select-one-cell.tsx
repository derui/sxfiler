import { h, Fragment } from "preact";
import { Item, SelectOption, ItemElementType } from "@/configurations/types";
import { useState } from "preact/hooks";

type Callback = (value: string) => void;
type OptionCallback = (option: SelectOption) => void;

export type Props = {
  item: Item;
  onUpdated: Callback;
};

const makeOption = (defaultValue: string, onClick: OptionCallback) => (option: SelectOption, index: number) => {
  return (
    <Fragment>
      <li
        class="configuration-cell__selection-menu-option"
        key={option.value}
        data-testid={`configuration-select-option-${index}`}
        data-selected={defaultValue === option.toString()}
        onClick={() => onClick(option)}
      >
        {option.display}
      </li>
      <div class="configuration-cell__option-separator" />
    </Fragment>
  );
};

export const Component: preact.FunctionComponent<Props> = ({ item, onUpdated }) => {
  if (item.type.kind !== ItemElementType.SELECT_ONE) {
    return null;
  }

  const [state, setState] = useState(item.type.defaultValue);
  const [showMenu, setShowMenu] = useState(false);
  const selected = item.type.options.find((v) => v.toString() === state);

  return (
    <div class="configuration-cell__root" data-testid="configuration-select-one-cell-root">
      <label class="configuration-cell__label">{item.displayName}</label>
      <div class="configuration-cell__select-container" data-testid="configuration-select-one-cell-container">
        <div
          class="configuration-cell__select-input"
          data-testid="configuration-select"
          onClick={() => setShowMenu(!showMenu)}
        >
          <span class="configuration-cell__placeholder">{selected ? selected.toString() : "---"}</span>
          <span class="configuration-cell__dropdown-mark" />
        </div>
        <ul class="configuration-cell__selection-menu-container" data-opened={showMenu}>
          {item.type.options.map(
            makeOption(state, (item) => {
              setShowMenu(false);
              setState(item.toString());
              onUpdated(item.toString());
            })
          )}
        </ul>
      </div>
      <p class="configuration-cell__description">{item.description}</p>
    </div>
  );
};
