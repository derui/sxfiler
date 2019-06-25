import * as React from "react";

import * as Element from "../../ui/element/element";
import * as FileListContainer from "./file-list-container/file-list-container";
import * as NotificationContainer from "./notification-container/notification-container";
import * as LogViewerContainer from "./log-viewer-container/log-viewer-container";
import { Component as RootRef } from "../../ui/root-ref/root-ref";

import LocatorContext, { Locator } from "../../../locator";
import { AppState } from "../../../states";
import { findBinding } from "../../../states/keymap";
import * as kbd from "../../../libs/kbd";
import * as SuggestionModalContainer from "./suggestion-modal-container/suggestion-modal-container";

const styles = require("./main-container.module.scss");

export interface Props {
  state: AppState;
}

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
      <LocatorContext.Consumer>
        {locator => (
          <RootRef rootRef={this.layoutRef}>
            <Element.Component
              className={styles.root}
              tabIndex={0}
              onKeyDown={handleKeyDown(locator, this.props.state)}
            >
              <FileListContainer.Component key="filer" state={fileList} />
              <NotificationContainer.Component key="notification" state={notification} />
              <LogViewerContainer.Component key="log" state={logEntry} />
              <SuggestionModalContainer.Component state={this.props.state.taskInteraction} />
            </Element.Component>
          </RootRef>
        )}
      </LocatorContext.Consumer>
    );
  }
}
