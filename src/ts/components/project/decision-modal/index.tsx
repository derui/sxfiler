import { h, Fragment } from "preact";
import * as Modal from "@/components/ui/modal";
import * as OverwritePanel from "./overwrite-panel";
import * as RenamePanel from "./rename-panel";
import { SelectableAction, DecisionAction } from "@/modules/decision/reducer";

export type ContainerProps = {
  focusedActionKind?: DecisionAction;
  selectableActions: SelectableAction[];
  onNewNameChange: (newName: string) => void;
};

const makeDecisionPanel = (
  focused: DecisionAction | undefined,
  selectableActions: SelectableAction[],
  handleRename: (reply: any) => void
) => {
  return (
    <Fragment>
      {selectableActions.map((v) => {
        switch (v.kind) {
          case DecisionAction.Overwrite:
            return <OverwritePanel.Component key="overwrite" selected={focused === DecisionAction.Overwrite} />;
          case DecisionAction.Rename:
            return (
              <RenamePanel.Component
                key="rename"
                selected={focused === DecisionAction.Rename}
                onUpdated={handleRename}
                itemName={v.newName}
              />
            );
          default:
            return null;
        }
      })}
    </Fragment>
  );
};

const Container: preact.FunctionComponent<ContainerProps> = ({
  focusedActionKind,
  onNewNameChange,
  selectableActions,
}) => {
  return (
    <div class="decision-modal__inner-container" data-testid="decisionModal">
      <h4 class="decision-modal__header" data-testid="decisionModal-header">
        Decisions
      </h4>
      <section class="decision-modal__panel-container" data-testid="decisionModal-panelContainer">
        {makeDecisionPanel(focusedActionKind, selectableActions, onNewNameChange)}
      </section>
    </div>
  );
};

export type Props = Modal.Props<ContainerProps>;

export const Component = Modal.createComponent({
  container: Container,
});
