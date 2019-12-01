import * as React from "react";
import { styled } from "@/components/theme";
import * as Modal from "@/components/ui/modal";
import * as OverwriteSuggestionPanel from "./overwrite-suggestion-panel";
import * as RenameSuggestionPanel from "./rename-suggestion-panel";

import * as Element from "@/components/ui/element";

import { ReplyPayload, ReplyKind } from "@/domains/task-reply";
import { Transition } from "react-transition-group";

export type OverlayProps = { className?: string };

export type ContainerProps = {
  className?: string;
  focusedReply: number;
  replies: ReplyPayload[];
  onReply: (reply: ReplyPayload) => void;
};

type ContainerContextProps = ContainerProps & {
  opened: boolean;
  onClose: () => void;
  onOpen: () => void;
};

type OverlayContextProps = OverlayProps & {
  opened: boolean;
};

const Root = styled(Element.Component)`
  ${Modal.rootStyle}
  overflow: hidden;
  top: 0;
`;

const InnerContainer = styled.div`
  ${Modal.containerStyle};
  display: grid;
  grid-template-rows: auto auto;
  grid-template-columns: auto;
  flex: 0 1 auto;
  overflow: hidden;
  margin: 0px auto auto auto;

  border-radius: 0px 0px ${props => props.theme.spaces.small} ${props => props.theme.spaces.small};
  box-shadow: ${props => props.theme.boxShadow};

  &[data-state="entering"] {
    transform: translateY(-100%);
  }

  &[data-state="entered"] {
    transform: translateY(0);
    transition: transform ease-out 200ms;
  }

  &[data-state="exited"] {
    transform: translateY(0);
  }

  &[data-state="exiting"] {
    transition: transform ease-in 200ms;
    transform: translateY(-100%);
  }
`;

const Header = styled.h4`
  margin: 0;
  color: ${props => props.theme.colors.base03};
  background-color: ${props => props.theme.colors.base3};
  padding: ${props => props.theme.spaces.large};
  box-shadow: ${props => props.theme.headerShadow};
`;

const PanelContainer = styled.section`
  padding: ${props => props.theme.spaces.base};
  background-color: ${props => props.theme.colors.base03};
`;

const InnerOverlay = styled(Element.Component)`
  ${Modal.overlayStyle};

  background-color: rgba(0, 0, 0, 0.2);

  &[data-state="entering"] {
    opacity: 0.1;
  }

  &[data-state="entered"] {
    opacity: 1;
    transition: opacity ease-out 200ms;
  }
`;

const Overlay: React.FC<OverlayContextProps> = ({ opened }) => {
  return (
    <Transition in={opened} timeout={100} unmountOnExit={true}>
      {state => <InnerOverlay data-state={state} />}
    </Transition>
  );
};

const makeSuggestionPanel = (index: number, replies: ReplyPayload[], handleReply: (reply: ReplyPayload) => void) => {
  return replies.map((v, i) => {
    switch (v.kind) {
      case ReplyKind.Overwrite:
        return <OverwriteSuggestionPanel.Component key={i} selected={index === i} />;
      case ReplyKind.Rename:
        return (
          <RenameSuggestionPanel.Component
            key={i}
            selected={index === i}
            onUpdated={handleReply}
            nodeName={v.newName}
          />
        );
    }
  });
};

const Container: React.FC<ContainerContextProps> = ({ replies, focusedReply, onReply, opened, onClose, onOpen }) => {
  return (
    <Transition in={opened} timeout={200} onEnter={onOpen} onExited={onClose}>
      {state => {
        return (
          <InnerContainer data-state={state}>
            <Header>Suggestions</Header>
            <PanelContainer>{makeSuggestionPanel(focusedReply, replies, onReply)}</PanelContainer>
          </InnerContainer>
        );
      }}
    </Transition>
  );
};

export type Props = Modal.Props<ContainerProps, OverlayProps>;

export const Component = Modal.createComponent({
  root: Root,
  container: Container,
  overlay: Overlay,
});
