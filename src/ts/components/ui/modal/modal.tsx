import * as React from "react";
import * as ReactDOM from "react-dom";
import * as Element from "../element/element";

export interface ModalClassNames {
  root?: string;
  overlay?: string;
  container?: string;
}

export type Props<
  ContainerProps extends { className?: string } = Element.Props,
  OverlayProps extends { className?: string } = Element.Props,
  T extends Element.Props<H> = Element.Props,
  H extends HTMLElement = HTMLElement
> = T &
  React.HTMLAttributes<H> & {
    dialogRoot: HTMLElement;
    opened: boolean;
    container?: ContainerProps;
    overlay?: OverlayProps;
  };

export function createComponent<
  ContainerProps extends { className?: string } = Element.Props,
  OverlayProps extends { className?: string } = Element.Props,
  T extends Element.Props<H> = Element.Props,
  H extends HTMLElement = HTMLElement
>(
  context: {
    container?: React.ComponentType<ContainerProps>;
    overlay?: React.ComponentType<OverlayProps>;
    classNames?: ModalClassNames;
  } = {}
): React.ComponentType<Props<ContainerProps, OverlayProps, T, H>> {
  const modalClassNames = context.classNames || {};
  const Container = context.container || Element.Component;
  const Overlay = context.overlay || Element.Component;

  return class Dialog extends React.Component<Props<ContainerProps, OverlayProps, T, H>> {
    constructor(props: Props<ContainerProps, OverlayProps, T, H>) {
      super(props);
    }

    render() {
      const {
        opened,
        dialogRoot,
        overlay,
        container,
        ...props
      }: Props<ContainerProps, OverlayProps, T, H> = this.props;
      if (!opened) {
        return null;
      }

      return ReactDOM.createPortal(
        <Element.Component className={modalClassNames.root} {...props}>
          {overlay && <Overlay className={modalClassNames.overlay} {...overlay} />}
          {container && <Container className={modalClassNames.container} {...container} />}
        </Element.Component>,
        this.props.dialogRoot
      );
    }
  };
}

export const Component = createComponent();
