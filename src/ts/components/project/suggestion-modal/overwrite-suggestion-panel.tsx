import * as React from "react";
import * as Element from "../../ui/element/element";

const style: ClassNames = require("./overwrite-suggestion-panel.module.scss");

type ClassNames = {
  root: string;
  text: string;
};

export type Props = {
  selected: boolean;
};

export const Component: React.FC<Props> = ({ selected }) => {
  return (
    <Element.Component className={style.root} aria-selected={selected}>
      <p className={style.text}>Overwrite</p>
    </Element.Component>
  );
};
