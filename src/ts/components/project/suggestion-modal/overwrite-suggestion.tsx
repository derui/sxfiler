import * as React from "react";
import { Suggestion, SuggestionKind } from "../../../domains/task-suggestion";
import * as Element from "../../ui/element/element";
import { ReplyPayload, createOverwritePayload } from "../../../domains/task-reply";

const style: ClassNames = require("./overwrite-suggestion.module.scss");

type ClassNames = {
  root: string;
  text: string;
};

type Handler = (payload: ReplyPayload) => void;

export type Props = {
  selected: boolean;
  suggestion: Suggestion;
  onReply: Handler;
};

const handleKeyDown = (selected: boolean, onReply: Handler) => (ev: React.KeyboardEvent) => {
  if (!selected) {
    return;
  }

  if (ev.key === "Enter") {
    ev.stopPropagation();
    ev.preventDefault();
    onReply(createOverwritePayload());
  }
};

export const Component: React.FC<Props> = ({ selected, suggestion, onReply }) => {
  if (suggestion.kind !== SuggestionKind.Overwrite) {
    return null;
  }

  return (
    <Element.Component className={style.root} onKeyDown={handleKeyDown(selected, onReply)}>
      <p className={style.text} aria-selected={selected}>{`Overwrite ${suggestion.nodeName}.`}</p>
    </Element.Component>
  );
};
