import * as React from "react";

import { styled } from "@/components/theme";
import * as Element from "@/components/ui/element";
import * as FileListContainer from "./file-list-container";
import * as NotificationContainer from "./notification-container";
import * as LogViewerContainer from "./log-viewer-container";
import * as HistorySelectorContainer from "./history-selector-container";

import { LocatorContext, Locator } from "@/locator";
import { AppState } from "@/states";
import { findBinding } from "@/states/keymap";
import * as kbd from "@/libs/kbd";
import * as SuggestionModalContainer from "./suggestion-modal-container";

export type Props = {
  state: AppState;
};

const Root = styled(Element.Component)`
  max-width: 100%;
  min-width: 0;
  max-height: 100%;
  height: 100%;
  margin: 0;
  padding: 0;

  overflow: hidden;
  background-color: ${props => props.theme.colors.base03};

  display: grid;
  grid-template-rows: 1fr 10% auto;
`;

/**
 * handle keyboard event that all keydown event on application
 * @param props properties of component
 */
function handleKeyDown(locator: Locator, state: AppState) {
  return (ev: React.KeyboardEvent<any>) => {
    const { context, commandRegistrar } = locator;

    switch (ev.type) {
      case "keydown": {
        const key = kbd.make(ev.key, { meta: ev.metaKey, ctrl: ev.ctrlKey });
        const binding = findBinding(state.keymap, state.context, kbd.toKeySeq(key));

        if (!binding) {
          break;
        }

        ev.preventDefault();
        ev.stopPropagation();
        if (context && commandRegistrar) {
          commandRegistrar.execute(binding.action, context, { state });
        }
        break;
      }
      default:
        break;
    }

    return;
  };
}

export const Component: React.FC<Props> = ({ state }) => {
  const layoutRef: React.RefObject<HTMLElement> = React.useRef(null);

  React.useEffect(() => {
    if (layoutRef.current) {
      layoutRef.current.focus();
    }
  }, [layoutRef.current]);

  const { fileList, notification, logEntry, taskInteraction, history } = state;
  const locator = React.useContext(LocatorContext);

  return (
    <Root ref={layoutRef} tabIndex={0} onKeyDown={handleKeyDown(locator, state)}>
      <FileListContainer.Component state={fileList} />
      <LogViewerContainer.Component state={logEntry} />
      <NotificationContainer.Component state={notification} />
      <SuggestionModalContainer.Component state={taskInteraction} />
      <HistorySelectorContainer.Component state={history} />
    </Root>
  );
};
