import * as React from "react";
import * as Modal from "../../ui/modal/modal";
import * as OverwriteSuggestionPanel from "./overwrite-suggestion-panel";
import * as RenameSuggestionPanel from "./rename-suggestion-panel";
import { ReplyPayload, ReplyKind } from "../../../domains/task-reply";
import { CSSTransition } from "react-transition-group";

const style: ClassNames = require("./suggestion-modal.module.scss");

type ClassNames = Modal.ModalClassNames & {
  header: string;
  panelContainer: string;
  originalNode: string;
  rootAnimationExit: string;
  rootAnimationExitActive: string;
  rootAnimationEnter: string;
  rootAnimationEnterActive: string;
  overlayAnimationEnter: string;
  overlayAnimationEnterActive: string;
};

export type OverlayProps = { className?: string };

export type ContainerProps = {
  className?: string;
  focusedReply: number;
  replies: ReplyPayload[];
  onReply: (reply: ReplyPayload) => void;
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

const makeSuggestionPanel = (index: number, replies: ReplyPayload[], handleReply: (reply: ReplyPayload) => void) => {
  return replies.map((v, i) => {
    switch (v.kind) {
      case ReplyKind.Overwrite:
        return <OverwriteSuggestionPanel.Component key={i} selected={index === i} />;
      case ReplyKind.Rename:
        return (
          <RenameSuggestionPanel.Component
            key={i}
            selected={index === i}
            onUpdated={handleReply}
            nodeName={v.newName}
          />
        );
    }
  });
};

const Container: React.FC<ContainerContextProps> = ({
  className,
  replies,
  focusedReply,
  onReply,
  opened,
  onClose,
  onOpen,
}) => {
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
            <h4 className={style.header}>Suggestions </h4>
            <section className={style.panelContainer}>{makeSuggestionPanel(focusedReply, replies, onReply)}</section>
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
