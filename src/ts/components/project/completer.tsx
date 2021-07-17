import { h } from "preact";
import * as Modal from "@/components/ui/modal";
import { useRef, useState, useEffect } from "preact/hooks";
import { SplittedCandidate } from "@/modules/completer/selectors";

type Item = SplittedCandidate;

export type ContainerProps = {
  class?: string;
  title: string;
  items: Item[];
  selectedItemIndex: number;
  onInput: (input: string) => void;
};

type ContainerContextProps = ContainerProps & {
  opened: boolean;
};

const handleInput =
  (cb: (input: string) => void, cb2: (input: string) => void) =>
  (e: preact.JSX.TargetedEvent<HTMLInputElement, Event>) => {
    cb(e.currentTarget.value || "");
    cb2(e.currentTarget.value || "");
  };

const scrollItemIfNeeded = (parent: HTMLElement | null | undefined, selected: boolean) => (e: HTMLElement | null) => {
  if (!e || !parent || !selected) {
    return;
  }

  const parentScrolledViewport = parent.offsetHeight + parent.scrollTop;
  const offsetBottom = e.offsetTop + e.offsetHeight;
  const viewedPartialBottom = e.offsetTop <= parent.scrollTop && parent.scrollTop <= offsetBottom;
  const viewedPartialTop = e.offsetTop <= parentScrolledViewport && parentScrolledViewport <= offsetBottom;

  if (viewedPartialBottom) {
    e.scrollIntoView(true);
  } else if (viewedPartialTop) {
    e.scrollIntoView(false);
  }
};

/**
 * Make list that contains completion items
 */
const makeList = (ref: preact.RefObject<HTMLDivElement>, items: Item[], index: number) => {
  const listItems = items.map((v, i) => {
    return (
      <div
        role="listitem"
        class="completer__candidate-item"
        data-testid="completer-candidateItem"
        aria-selected={i === index}
        key={v.id}
        ref={scrollItemIfNeeded(ref.current, i === index)}
      >
        {v.before}
        <span class="completer__matching-area" data-testid="completer-matchingArea">
          {v.matched}
        </span>
        {v.after}
      </div>
    );
  });

  return (
    <div role="list" class="completer__candidate-list" data-testid="completer-candidateList" ref={ref}>
      {listItems}
    </div>
  );
};

const Container: preact.FunctionComponent<ContainerContextProps> = ({
  selectedItemIndex,
  items,
  onInput,
  title,
  opened,
}) => {
  const [state, setState] = useState("");
  const ref = useRef<HTMLDivElement>(null);
  const refInput = useRef<HTMLInputElement>(null);

  useEffect(() => {
    if (opened && refInput.current) {
      refInput.current.focus();
    }

    return () => {
      setState("");
    };
  }, [opened]);

  return (
    <div class="completer__container" data-testid="completer-container" data-state={"entered"}>
      <h4 class="completer__title" data-testid="completer-title">
        {title}
      </h4>
      <div class="completer__input-container" data-testid="completer-inputContainer">
        <input
          class="completer__input"
          data-testid="completer-input"
          type="text"
          onInput={handleInput(onInput, setState)}
          value={state}
          ref={refInput}
        />
      </div>
      {makeList(ref, items, selectedItemIndex)}
    </div>
  );
};

export type Props = ContainerProps & {
  dialogRoot: HTMLElement;
  opened: boolean;
};

const CustomModal = Modal.createComponent({
  container: Container,
});

export const Component = ({ dialogRoot, opened, ...rest }: Props) => (
  <CustomModal position="top" dialogRoot={dialogRoot} opened={opened} container={rest} />
);
