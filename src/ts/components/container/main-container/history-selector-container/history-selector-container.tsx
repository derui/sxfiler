import * as React from "react";

import { Component as Completer } from "@/components/project/completer";
import * as H from "@/states/history";
import ModalRootContext from "@/modal-root";

export type Props = {
  state: H.State;
};

const handleInput = (input: string) => {
  console.log(input);
};

export const Component: React.FC<Props> = ({ state }) => {
  return (
    <ModalRootContext.Consumer>
      {modalRoot => {
        if (!modalRoot.element) {
          return null;
        }

        return (
          <Completer
            dialogRoot={modalRoot.element}
            opened={state.opened}
            container={{
              title: "History",
              items: state.completion.candidates,
              selectedItemIndex: state.completion.cursor,
              onInput: handleInput,
            }}
          />
        );
      }}
    </ModalRootContext.Consumer>
  );
};
