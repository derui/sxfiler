import * as React from "react";
import { Suggestion, SuggestionKind } from "../../../domains/task-suggestion";
import * as Element from "../../ui/element/element";
import { ReplyPayload, createRenamePayload } from "../../../domains/task-reply";

const style: ClassNames = require("./rename-suggestion-panel.module.scss");

type ClassNames = {
  root: string;
  text: string;
  label: string;
  input: string;
  labelContainer: string;
};

type Handler = (payload: ReplyPayload) => void;

export type Props = {
  selected: boolean;
  suggestion: Suggestion;
  onReply: Handler;
  nodeName: string;
};

type State = {
  newName: string;
};

export class Component extends React.Component<Props, State> {
  constructor(props: Props) {
    super(props);

    this.state = { newName: props.nodeName };
  }

  private handleChange = () => (ev: React.ChangeEvent) => {
    this.setState({ newName: ev.target.nodeValue || "" });
  };

  private handleKeyDown = () => (ev: React.KeyboardEvent) => {
    if (!this.props.selected || this.state.newName === "") {
      return;
    }

    if (ev.key === "Enter") {
      ev.stopPropagation();
      ev.preventDefault();
      this.props.onReply(createRenamePayload(this.state.newName));
    }
  };

  render() {
    const { selected, suggestion } = this.props;
    if (suggestion.kind !== SuggestionKind.Rename) {
      return null;
    }

    return (
      <Element.Component className={style.root} onKeyDown={this.handleKeyDown()}>
        <p className={style.text} aria-selected={selected}>{`Rename`}</p>
        <label className={style.labelContainer} aria-selected={selected}>
          <span className={style.label}>New Name</span>
          <input className={style.input} type="text" value={this.state.newName} onChange={this.handleChange()} />
        </label>
      </Element.Component>
    );
  }
}
