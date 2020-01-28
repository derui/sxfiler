import { h } from "preact";

export type Props = {
  selected: boolean;
};

export const Component: preact.FunctionComponent<Props> = ({ selected }) => {
  return (
    <div class="decision-modal__panel" data-testid="decisionModal-overwritePanel" aria-selected={selected}>
      <p class="decision-modal__label" data-testid="decisionModal-overwriteLabel">
        Overwrite
      </p>
    </div>
  );
};
