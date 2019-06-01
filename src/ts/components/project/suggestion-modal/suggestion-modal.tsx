import * as React from "react";
import * as Modal from "../../ui/modal/modal";
import { Suggestion, SuggestionKind } from "../../../domains/task-suggestion";
import * as OverwriteSuggestionPanel from "./overwrite-suggestion-panel";
import * as RenameSuggestionPanel from "./rename-suggestion-panel";
import { ReplyPayload } from "../../../domains/task-reply";
import { CSSTransition } from "react-transition-group";

const style: ClassNames = require("./suggestion-modal.module.scss");

type ClassNames = Modal.ModalClassNames & {
  header: string;
  panelContainer: string;
  rootAnimationExit: string;
  rootAnimationExitActive: string;
  rootAnimationEnter: string;
  rootAnimationEnterActive: string;
};

type OverlayProps = { className?: string };

export type ContainerProps = {
  className?: string;
  nodeName: string;
  focusedSuggestion: number;
  suggestions: Suggestion[];
  onReply: (reply: ReplyPayload) => void;
};

type ContainerContextProps = ContainerProps & {
  opened: boolean;
  onClose: () => void;
  onOpen: () => void;
};

const makeSuggestionPanel = (
  nodeName: string,
  index: number,
  suggestions: Suggestion[],
  handleReply: (reply: ReplyPayload) => void
) => {
  return suggestions.map((v, i) => {
    switch (v.kind) {
      case SuggestionKind.Overwrite:
        return (
          <OverwriteSuggestionPanel.Component key={i} selected={index === i} suggestion={v} onReply={handleReply} />
        );
      case SuggestionKind.Rename:
        return (
          <RenameSuggestionPanel.Component
            key={i}
            nodeName={nodeName}
            selected={index === i}
            suggestion={v}
            onReply={handleReply}
          />
        );
    }
  });
};

const Container: React.FC<ContainerContextProps> = ({
  className,
  nodeName,
  suggestions,
  focusedSuggestion,
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
            <h4 className={style.header}>Suggestions for {nodeName}</h4>
            <section className={style.panelContainer}>
              {makeSuggestionPanel(nodeName, focusedSuggestion, suggestions, onReply)}
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
});
