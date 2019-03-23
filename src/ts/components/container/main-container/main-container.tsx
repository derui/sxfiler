import * as React from "react";

import * as Element from "../../ui/element/element";
import * as FileListContainer from "./file-list-container/file-list-container";
import * as NotificationContainer from "./notification-container/notification-container";
import { Component as RootRef } from "../../ui/root-ref/root-ref";

import { ContextLike } from "../../../context";
import LocatorContext from "../../../locator";
import { StoreState } from "../../../types/store-state";
import { State as KeymapState, findBinding } from "../../../types/store-state/keymap";
import * as kbd from "../../../libs/kbd";

const styles = require("./main-container.module.scss");

export interface Props {
  state: StoreState;
}

/**
 * handle keyboard event that all keydown event on application
 * @param props properties of component
 */
function handleKeyDown(context: ContextLike | undefined, keymap: KeymapState): (ev: React.KeyboardEvent<any>) => void {
  return ev => {
    if (!context) {
      return;
    }

    switch (ev.type) {
      case "keydown": {
        const key = kbd.make(ev.key, { meta: ev.metaKey, ctrl: ev.ctrlKey });
        const binding = findBinding(keymap, kbd.toKeySeq(key));

        if (!binding) {
          break;
        }

        ev.preventDefault();
        ev.stopPropagation();
        // TODO: implement to invoke action
        /* onAction(binding.action); */
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
    const { fileList, notification, keymap } = this.props.state;
    return (
      <LocatorContext.Consumer>
        {({ context }) => (
          <RootRef rootRef={this.layoutRef}>
            <Element.Component className={styles.root} tabIndex={0} onKeyDown={handleKeyDown(context, keymap)}>
              <FileListContainer.Component key="filer" state={fileList} />
              <NotificationContainer.Component key="notification" state={notification} />
            </Element.Component>
          </RootRef>
        )}
      </LocatorContext.Consumer>
    );
  }
}
