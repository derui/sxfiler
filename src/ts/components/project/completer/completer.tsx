import * as React from "react";
import * as Modal from "../../ui/modal/modal";
import * as List from "../../ui/list/list";
import * as ListItem from "../../ui/list-item/list-item";
import { CSSTransition } from "react-transition-group";
import { Candidate } from "../../../domains/candidate";

const style: ClassNames = require("./completer.module.scss");

type ClassNames = Modal.ModalClassNames & {
  overlay: string;
  title: string;
  itemContainer: string;
  inputContainer: string;
  matching: string;
  input: string;
  list: string;
  listItem: string;
  rootAnimationExit: string;
  rootAnimationExitActive: string;
  rootAnimationEnter: string;
  rootAnimationEnterActive: string;
  overlayAnimationEnter: string;
  overlayAnimationEnterActive: string;
};

export type OverlayProps = { className?: string };

type Item = Candidate;

export type ContainerProps = {
  className?: string;
  title: string;
  items: Item[];
  selectedItemIndex: number;
  onInput: (input: string) => void;
};

type ContainerContextProps = ContainerProps & {
  opened: boolean;
  onClose: () => void;
  onOpen: () => void;
};

type OverlayContextProps = OverlayProps & {
  opened: boolean;
};

const Overlay: React.FC<OverlayContextProps> = ({ className, opened }) => {
  return (
    <CSSTransition
      in={opened}
      timeout={100}
      unmountOnExit={true}
      classNames={{
        enter: style.overlayAnimationEnter,
        enterActive: style.overlayAnimationEnterActive,
      }}
    >
      {() => <div className={className} />}
    </CSSTransition>
  );
};

const handleChange = (cb: (input: string) => void, cb2: (input: string) => void) => (
  e: React.ChangeEvent<HTMLInputElement>
): void => {
  cb(e.target.value || "");
  cb2(e.target.value || "");
};

/**
 * Make list that contains completion items
 */
const makeList = (items: Item[], index: number) => {
  const listItems = items.map((v, i) => {
    const [before, matched, after] = v.splitByInput();
    return (
      <ListItem.Component className={style.listItem} selected={i === index} key={v.id}>
        {before}
        <span className={style.matching}>{matched}</span>
        {after}
      </ListItem.Component>
    );
  });
  return <List.Component className={style.list}>{listItems}</List.Component>;
};

const Container: React.FC<ContainerContextProps> = ({
  className,
  selectedItemIndex,
  items,
  onInput,
  title,
  opened,
  onClose,
  onOpen,
}) => {
  const [state, setState] = React.useState("");

  return (
    <CSSTransition
      in={opened}
      timeout={200}
      onEnter={onOpen}
      onExited={onClose}
      classNames={{
        enter: style.rootAnimationEnter,
        enterActive: style.rootAnimationEnterActive,
        exit: style.rootAnimationExit,
        exitActive: style.rootAnimationExitActive,
      }}
    >
      {() => {
        return (
          <div className={className}>
            <h4 className={style.title}>{title}</h4>
            <section className={style.itemContainer}>
              <div className={style.inputContainer}>
                <input className={style.input} type="text" onChange={handleChange(onInput, setState)} value={state} />
              </div>
              {makeList(items, selectedItemIndex)}
            </section>
          </div>
        );
      }}
    </CSSTransition>
  );
};

export type Props = Modal.Props<ContainerProps, OverlayProps>;

export const Component = Modal.createComponent({
  classNames: style,
  container: Container,
  overlay: Overlay,
});
