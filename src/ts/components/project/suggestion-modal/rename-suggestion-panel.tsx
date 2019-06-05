import * as React from "react";
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
  onUpdated: Handler;
  nodeName: string;
};

const handleChange = (handler: Handler) => (ev: React.ChangeEvent) =>
  handler(createRenamePayload(ev.target.nodeValue || ""));

export const Component: React.FC<Props> = ({ selected, nodeName, onUpdated }) => {
  return (
    <Element.Component className={style.root}>
      <p className={style.text} aria-selected={selected}>{`Rename`}</p>
      <label className={style.labelContainer} aria-selected={selected}>
        <span className={style.label}>New Name</span>
        <input className={style.input} type="text" value={nodeName} onChange={handleChange(onUpdated)} />
      </label>
    </Element.Component>
  );
};
