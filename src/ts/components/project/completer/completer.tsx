import * as React from "react";
import * as Modal from "@/components/ui/modal";
import * as List from "@/components/ui/list";
import * as Element from "@/components/ui/element";
import * as ListItem from "@/components/ui/list-item";
import { Transition } from "react-transition-group";
import { Candidate, splitByMatching } from "@/domains/candidate";
import { styled } from "@/components/theme";

type Item = Candidate;

export type OverlayProps = { className?: string };

export type ContainerProps = {
  className?: string;
  title: string;
  items: Item[];
  selectedItemIndex: number;
  onInput: (input: string) => void;
};

type ContainerContextProps = ContainerProps & {
  opened: boolean;
  onClose: () => void;
  onOpen: () => void;
};

type OverlayContextProps = OverlayProps & {
  opened: boolean;
};

const root = styled(Element.Component)`
  ${Modal.rootStyle}
  top: 0;
`;

/// styled components.
const InnerOverlay = styled.div`
  ${Modal.overlayStyle};

  background-color: rgba(0, 0, 0, 0.2);

  &[data-state="entering"] {
    opacity: 0.1;
  }

  &[data-state="entered"] {
    opacity: 1;
    transition: opacity ease-out 200ms;
  }

  &[data-state="exiting"] {
    opacity: 0.1;
    transition: opacity ease-out 200ms;
  }
`;

const CandidateList = styled(List.Component)`
  ${List.style};

  overflow: hidden;
  overflow-y: auto;
`;

const CandidateItem = styled(ListItem.Component)`
  ${ListItem.style};
  padding: ${({ theme }) => `${theme.spaces.small} ${theme.spaces.base}`};
  color: ${({ theme }) => theme.colors.base3};
  font-size: 0.8rem;

  &[aria-selected="true"] {
    background-color: ${({ theme }) => theme.colors.base2}30;
  }
`;

const MatchingArea = styled.span`
  border-radius: ${props => props.theme.baseBorderRadius};

  color: ${({ theme }) => theme.colors.base03};
  background-color: ${({ theme }) => theme.colors.yellow};
`;

const InnerContainer = styled.div`
  ${Modal.containerStyle};
  display: grid;
  grid-template-rows: auto auto 1fr;
  grid-template-columns: 100%;
  flex: 0 1 auto;
  overflow: hidden;

  margin: 0px auto auto auto;
  width: 50%;
  max-height: 80%;

  border-radius: ${props => props.theme.spaces.small};
  background-color: ${props => props.theme.colors.base02};

  box-shadow: ${props => props.theme.boxShadow};

  &[data-state="entering"] {
    transform: translateY(-100%);
  }

  &[data-state="entered"] {
    transform: translateY(0);
    transition: transform ease-out 200ms;
  }

  &[data-state="exiting"] {
    transition: transform ease-in 200ms;
    transform: translateY(-100%);
  }
`;

const Title = styled.h4`
  color: ${props => props.theme.colors.base03};
  background-color: ${props => props.theme.colors.base3};
  padding: ${props => props.theme.spaces.base};
  margin: 0px;
  box-shadow: ${props => props.theme.headerShadow};
`;

const InputContainer = styled.div`
  display: flex;
  margin-top: ${props => props.theme.spaces.small};
  padding: ${props => props.theme.spaces.small};
  padding-bottom: ${props => props.theme.spaces.small};
  height: 1.5rem;
`;

const Input = styled.input`
  flex: 1 1 auto;

  outline: none;
  border: 1px solid ${props => props.theme.colors.blue};

  font-size: 1rem;
  color: ${props => props.theme.colors.base3};
  background-color: ${props => props.theme.colors.base03};
`;

const handleChange = (cb: (input: string) => void, cb2: (input: string) => void) => (
  e: React.ChangeEvent<HTMLInputElement>
): void => {
  cb(e.target.value || "");
  cb2(e.target.value || "");
};

/**
 * Make list that contains completion items
 */
const makeList = (items: Item[], index: number) => {
  const listItems = items.map((v, i) => {
    const [before, matched, after] = splitByMatching(v);
    return (
      <CandidateItem selected={i === index} key={v.id}>
        {before}
        <MatchingArea>{matched}</MatchingArea>
        {after}
      </CandidateItem>
    );
  });
  return <CandidateList>{listItems}</CandidateList>;
};

const Overlay: React.FC<OverlayContextProps> = ({ opened }) => {
  return (
    <Transition in={opened} timeout={200}>
      {state => <InnerOverlay data-state={state} />}
    </Transition>
  );
};

const Container: React.FC<ContainerContextProps> = ({
  selectedItemIndex,
  items,
  onInput,
  title,
  opened,
  onClose,
  onOpen,
}) => {
  const [state, setState] = React.useState("");

  return (
    <Transition in={opened} timeout={200} onEnter={onOpen} onExited={onClose}>
      {transitionState => {
        return (
          <InnerContainer data-state={transitionState}>
            <Title>{title}</Title>
            <InputContainer>
              <Input type="text" onChange={handleChange(onInput, setState)} value={state} />
            </InputContainer>
            {makeList(items, selectedItemIndex)}
          </InnerContainer>
        );
      }}
    </Transition>
  );
};

export type Props = Modal.Props<ContainerProps, OverlayProps>;

export const Component = Modal.createComponent({
  root,
  container: Container,
  overlay: Overlay,
});
