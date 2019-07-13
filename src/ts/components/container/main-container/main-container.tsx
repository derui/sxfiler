import * as React from "react";

import { styled, Theme, ThemeProvider } from "@/components/theme";
import * as Element from "@/components/ui/element/element";
import * as FileListContainer from "./file-list-container/file-list-container";
import * as NotificationContainer from "./notification-container/notification-container";
import * as LogViewerContainer from "./log-viewer-container/log-viewer-container";
import { Component as RootRef } from "@/components/ui/root-ref/root-ref";

import LocatorContext, { Locator } from "@/locator";
import { AppState } from "@/states";
import { findBinding } from "@/states/keymap";
import * as kbd from "@/libs/kbd";
import * as SuggestionModalContainer from "./suggestion-modal-container/suggestion-modal-container";

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
function handleKeyDown(locator: Locator, state: AppState): (ev: React.KeyboardEvent<any>) => void {
  return ev => {
    const { context, commandRegistrar } = locator;

    switch (ev.type) {
      case "keydown": {
        const key = kbd.make(ev.key, { meta: ev.metaKey, ctrl: ev.ctrlKey });
        const binding = findBinding(state.keymap, kbd.toKeySeq(key));

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

export class Component extends React.Component<Props> {
  private layoutRef: React.RefObject<HTMLElement> = React.createRef();

  public componentDidMount() {
    if (this.layoutRef.current) {
      this.layoutRef.current.focus();
    }
  }

  public componentDidUpdate() {
    if (this.layoutRef.current) {
      this.layoutRef.current.focus();
    }
  }

  public render() {
    const { fileList, notification, logEntry } = this.props.state;
    return (
      <ThemeProvider theme={Theme}>
        <LocatorContext.Consumer>
          {locator => (
            <RootRef rootRef={this.layoutRef}>
              <Root tabIndex={0} onKeyDown={handleKeyDown(locator, this.props.state)}>
                <FileListContainer.Component key="filer" state={fileList} />
                <LogViewerContainer.Component key="log" state={logEntry} />
                <NotificationContainer.Component key="notification" state={notification} />
                <SuggestionModalContainer.Component state={this.props.state.taskInteraction} />
              </Root>
            </RootRef>
          )}
        </LocatorContext.Consumer>
      </ThemeProvider>
    );
  }
}
