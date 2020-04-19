import { h } from "preact";

import { Component as Completer } from "@/components/project/completer";

import * as C from "@/modules";
import { ModalRootContext } from "@/modal-root";
import { LocatorContext, Locator } from "@/locator";
import { useContext } from "preact/hooks";
import { getSplittedByMatching } from "@/modules/completer/selectors";
import * as Commands from "@/commands/internal";

export type Props = {
  state: C.State;
};

const handleInput = (locator: Locator, state: C.State) => (input: string) => {
  const command = locator.commandResolver?.resolveBy(Commands.descriptors.completerComplete);
  if (!command) {
    return;
  }

  locator.commandExecutor?.execute(command, state, { input });
};

export const Component: preact.FunctionComponent<Props> = ({ state }) => {
  const { completer } = state;
  const locator = useContext(LocatorContext);
  if (!locator) {
    return null;
  }

  const modalRoot = useContext(ModalRootContext);
  if (!modalRoot.element) {
    return null;
  }

  return (
    <Completer
      dialogRoot={modalRoot.element}
      opened={completer.opened}
      title={completer.title || ""}
      items={getSplittedByMatching(completer)}
      selectedItemIndex={completer.currentCursorPosition.value}
      onInput={handleInput(locator, state)}
    />
  );
};
