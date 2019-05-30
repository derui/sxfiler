import * as React from "react";
import * as Modal from "../../ui/modal/modal";
import { Suggestion, SuggestionKind } from "../../../domains/task-suggestion";
import * as OverwriteSuggestionPanel from "./overwrite-suggestion-panel";
import * as RenameSuggestionPanel from "./rename-suggestion-panel";
import { ReplyPayload } from "../../../domains/task-reply";

const style: Modal.ModalClassNames = require("./suggestion-modal.module.scss");

type ContainerProps = {
  className?: string;
  focusedSuggestion: number;
  suggestions: Suggestion[];
  onReply: (reply: ReplyPayload) => void;
};

const makeSuggestionPanel = (index: number, suggestions: Suggestion[], handleReply: (reply: ReplyPayload) => void) => {
  return suggestions.map((v, i) => {
    switch (v.kind) {
      case SuggestionKind.Overwrite:
        return (
          <OverwriteSuggestionPanel.Component key={i} selected={index === i} suggestion={v} onReply={handleReply} />
        );
      case SuggestionKind.Rename:
        return <RenameSuggestionPanel.Component key={i} selected={index === i} suggestion={v} onReply={handleReply} />;
    }
  });
};

const Container: React.FC<ContainerProps> = ({ className, suggestions, focusedSuggestion, onReply }) => {
  return <div className={className}>{makeSuggestionPanel(focusedSuggestion, suggestions, onReply)}</div>;
};

export type Props = Modal.Props<ContainerProps>;

export const Component = Modal.createComponent({
  classNames: style,
  container: Container,
});
