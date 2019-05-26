import * as React from "react";
import * as Modal from "../../ui/modal/modal";
import { Suggestions } from "../../../domains/task-suggestion";

const style: Modal.ModalClassNames = require("./suggestion-modal.module.scss");

type ContainerProps = {
  className?: string;
  suggestions: Suggestions;
};

const Container: React.SFC<ContainerProps> = ({ className, suggestions }) => {
  return <div className={className} />;
};

export type Props = Modal.Props<ContainerProps>;

export const Component = Modal.createComponent({
  classNames: style,
  container: Container,
});
