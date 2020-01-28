import { h } from "preact";

export type Props = {
  start: number;
  windowSize: number;
};

export const Component: preact.FunctionComponent<Props> = ({ start, windowSize }) => {
  const top = start * 100.0;
  const height = windowSize * 100.0;

  return (
    <div class="scroll-bar__root" data-testid="scrollBar-root">
      <div
        class="scroll-bar__bar"
        data-testid="scrollBar-bar"
        style={{
          top: `${top}%`,
          height: `${height}%`,
        }}
      />
    </div>
  );
};
