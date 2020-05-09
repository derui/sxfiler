import { h, Fragment } from "preact";
import { ObjectEnum } from "@/utils";
import { useState, useEffect } from "preact/hooks";

export const TransitionState = {
  Entering: "entering",
  Entered: "entered",
  Exiting: "exiting",
  Exited: "exited",
} as const;
export type TransitionState = ObjectEnum<typeof TransitionState>;

type renderer = (state: TransitionState) => preact.VNode<any>;

export type Props = {
  children: renderer;
  in: boolean;
  timeout: number;
};

const tstate = TransitionState;

export const Transition: preact.FunctionalComponent<Props> = (props) => {
  const { children, timeout } = props;
  const [state, updateState] = useState<TransitionState>(TransitionState.Exited);
  const currentIn = props.in;

  useEffect(() => {
    const [transitioning, transitioned] = currentIn
      ? [tstate.Entering, tstate.Entered]
      : [tstate.Exiting, tstate.Exited];

    updateState(transitioning);
    const handle = setTimeout(() => {
      updateState(transitioned);
    }, timeout);

    return () => clearTimeout(handle);
  }, [currentIn]);

  return <Fragment>{children(state)}</Fragment>;
};
