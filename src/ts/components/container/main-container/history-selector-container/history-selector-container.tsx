import * as React from "react";

import { Component as Completer } from "@/components/project/completer";
import * as H from "@/states/history";
import ModalRootContext from "@/modal-root";
import { LocatorContext, Locator } from "@/locator";
import { createUseCase } from "@/usecases/history/read";

export type Props = {
  state: H.State;
};

const handleInput = (locator: Locator) => (input: string) => {
  const { context, client } = locator;
  if (!context || !client) {
    return;
  }

  context.use(createUseCase(client)).execute({ input });
};

export const Component: React.FC<Props> = ({ state }) => {
  return (
    <LocatorContext.Consumer>
      {locator => {
        if (!locator) {
          return null;
        }
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
                    onInput: handleInput(locator),
                  }}
                  overlay={{}}
                />
              );
            }}
          </ModalRootContext.Consumer>
        );
      }}
    </LocatorContext.Consumer>
  );
};
