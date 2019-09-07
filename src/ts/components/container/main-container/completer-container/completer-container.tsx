import * as React from "react";

import { Component as Completer } from "@/components/project/completer";
import * as C from "@/states/completer";
import { ModalRootContext } from "@/modal-root";
import { LocatorContext, Locator } from "@/locator";
import { createUseCase } from "@/usecases/completer/read";

export type Props = {
  state: C.State;
};

const handleInput = (locator: Locator) => (input: string) => {
  const { context, client } = locator;
  if (!context || !client) {
    return;
  }

  context.use(createUseCase(client))({ input });
};

export const Component: React.FC<Props> = ({ state }) => {
  const locator = React.useContext(LocatorContext);
  if (!locator) {
    return null;
  }

  const modalRoot = React.useContext(ModalRootContext);
  if (!modalRoot.element) {
    return null;
  }

  return (
    <Completer
      dialogRoot={modalRoot.element}
      opened={state.opened}
      title={state.title}
      items={state.completion.candidates}
      selectedItemIndex={state.completion.cursor}
      onInput={handleInput(locator)}
    />
  );
};