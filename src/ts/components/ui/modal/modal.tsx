import * as React from "react";
import * as ReactDOM from "react-dom";
import { styled, css } from "@/components/theme";
import * as Element from "@/components/ui/element/element";

export const rootStyle = css`
  display: flex;
  position: absolute;
  width: 100%;
  height: 100%;
  justify-content: center;

  &[aria-hidden="true"] {
    display: none;
  }
`;

const DefaultRoot = styled(Element.Component)`
  ${rootStyle}
`;

export const overlayStyle = css`
  position: absolute;
  top: 0;
  left: 0;
  width: 100%;
  height: 100%;

  z-index: 1;
`;

const DefaultOverlay = styled(Element.Component)`
  ${overlayStyle}
`;

export const containerStyle = css`
  z-index: 2;
`;

const DefaultContainer = styled(Element.Component)`
  ${containerStyle}
`;

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

export const createComponent = function createComponent<
  ContainerProps extends { className?: string } = Element.Props,
  OverlayProps extends { className?: string } = Element.Props,
  T extends Element.Props<H> = Element.Props,
  H extends HTMLElement = HTMLElement
>(
  context: {
    root?: Element.ComponentType;
    container?: React.ComponentType<ContainerProps & { opened: boolean; onClose: () => void; onOpen: () => void }>;
    overlay?: React.ComponentType<OverlayProps & { opened: boolean }>;
  } = {}
): React.ComponentType<Props<ContainerProps, OverlayProps, T, H>> {
  const Root = context.root || DefaultRoot;
  const Container = context.container || DefaultContainer;
  const Overlay = context.overlay || DefaultOverlay;

  const render: React.FC<Props<ContainerProps, OverlayProps, T, H>> = ({
    opened,
    dialogRoot,
    overlay,
    container,
    ...props
  }) => {
    const [closed, setClosed] = React.useState(true);

    return ReactDOM.createPortal(
      <Root aria-hidden={!opened && closed} {...props}>
        {overlay && <Overlay opened={opened} {...overlay} />}
        {container && (
          <Container onOpen={() => setClosed(false)} onClose={() => setClosed(true)} opened={opened} {...container} />
        )}
      </Root>,
      dialogRoot
    );
  };
  render.displayName = "dialog";

  return render;
};

export const Component = createComponent();
