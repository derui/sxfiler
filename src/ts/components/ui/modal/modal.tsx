import * as React from "react";
import * as ReactDOM from "react-dom";
import * as Element from "../element/element";
import { applyDisplayName } from "../util";

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
    container?: React.ComponentType<ContainerProps & { opened: boolean; onClose: () => void; onOpen: () => void }>;
    overlay?: React.ComponentType<OverlayProps & { opened: boolean }>;
    classNames?: ModalClassNames;
  } = {}
): React.ComponentType<Props<ContainerProps, OverlayProps, T, H>> {
  const modalClassNames = context.classNames || {};
  const Container = context.container || Element.Component;
  const Overlay = context.overlay || Element.Component;

  return applyDisplayName("Dialog", ({ opened, dialogRoot, overlay, container, ...props }) => {
    const [closed, setClosed] = React.useState(true);

    return ReactDOM.createPortal(
      <Element.Component className={modalClassNames.root} aria-hidden={!opened && closed} {...props}>
        {overlay && <Overlay className={modalClassNames.overlay} opened={opened} {...overlay} />}
        {container && (
          <Container
            className={modalClassNames.container}
            onOpen={() => setClosed(false)}
            onClose={() => setClosed(true)}
            opened={opened}
            {...container}
          />
        )}
      </Element.Component>,
      dialogRoot
    );
  });
}

export const Component = createComponent();
