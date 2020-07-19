import { h, Fragment } from "preact";
import { createPortal } from "preact/compat";
import classnames from "classnames";
import { Transition, TransitionState } from "./transition";

type BaseProps = preact.JSX.HTMLAttributes<HTMLElement> & {
  position?: "top" | undefined;
};

type BaseContainerProps = {
  opened: boolean;
};

type BaseOverlayProps = {
  opened: boolean;
  transition: TransitionState;
};

export type Props<ContainerProps extends object = {}, T extends BaseProps = BaseProps> = T & {
  dialogRoot: HTMLElement;
  opened: boolean;
  container?: ContainerProps;
};

const DefaultRoot: preact.FunctionComponent<BaseProps> = (props) => {
  const className = classnames({
    "ui-modal__root": !props.position,
    "ui-modal__root--top": props.position === "top" || !props.position,
  });

  return <div class={className} data-testid="modal-root" {...props} />;
};
DefaultRoot.displayName = "DefaultRoot";

const Overlay: preact.FunctionComponent<BaseOverlayProps> = ({ opened, transition }) =>
  opened ? <div class="ui-modal__overlay" data-state={transition} data-testid="modal-overlay" /> : null;
Overlay.displayName = "Overlay";

export const createComponent = function createComponent<
  ContainerProps extends object = {},
  T extends BaseProps = BaseProps
>(context: {
  root?: preact.ComponentType<BaseProps>;
  container: preact.ComponentType<ContainerProps & BaseContainerProps>;
}): preact.ComponentType<Props<ContainerProps, T>> {
  const Root = context.root || DefaultRoot;
  const Container = context.container;

  const render: preact.FunctionComponent<Props<ContainerProps, T>> = ({
    opened,
    dialogRoot,
    container,
    ...props
  }: Props<ContainerProps, T>) => {
    return createPortal(
      <Transition in={opened} timeout={200}>
        {(state) => {
          return (
            <Fragment>
              <Overlay opened={opened} transition={state} />
              <Root data-state={state} aria-hidden={!opened} {...props}>
                {container && h(Container, { ...container, opened })}
              </Root>
            </Fragment>
          );
        }}
      </Transition>,

      dialogRoot
    );
  };
  render.displayName = "Dialog";

  return render;
};
