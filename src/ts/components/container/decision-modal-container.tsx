import { h } from "preact";
import { useContext } from "preact/hooks";
import { ModalRootContext } from "@/modal-root";
import * as DecisionModal from "@/components/project/decision-modal";
import { LocatorContext } from "@/locator";
import { State } from "@/modules";
import { getCurrentFocusedActionKind } from "@/modules/decision/selectors";
import * as CommandResolver from "@/commands/command-resolver";
import * as CommandExecutor from "@/commands/command-executor";
import * as commands from "@/commands/internal";

export type Props = {
  state: State;
};

export type ElementType = ReturnType<typeof Component>;

const handleNewNameChange = (
  state: State,
  commandResolver: CommandResolver.Type,
  commandExecutor: CommandExecutor.Type
) => (newName: string) => {
  const command = commandResolver.resolveBy(commands.descriptors.decisionUpdateNewName);
  if (!command) {
    return;
  }
  commandExecutor.execute(command, state, { updatedName: newName });
};

// Stateless container to render filer
export const Component: preact.FunctionComponent<Props> = ({ state }): ElementType | null => {
  const { decision } = state;
  const { commandResolver, commandExecutor } = useContext(LocatorContext);
  const { element } = useContext(ModalRootContext);

  if (!commandResolver || !element || !commandExecutor) {
    return null;
  }
  const focusedActionKind = getCurrentFocusedActionKind(decision);

  return (
    <DecisionModal.Component
      dialogRoot={element}
      opened={decision.processing}
      container={{
        focusedActionKind,
        selectableActions: decision.selectableActions,
        onNewNameChange: handleNewNameChange(state, commandResolver, commandExecutor),
      }}
    />
  );
};
