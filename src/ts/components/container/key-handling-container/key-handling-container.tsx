import * as React from "react";

import * as kbd from "../../../libs/kbd";
import { State as KeymapState, findBinding } from "../../../types/store-state/keymap";
import * as Element from "../../ui/element/element";
import LocatorContext from "../../../locator";
import { ContextLike } from "../../../context";

const styles = require("./key-handling-container.module.scss");

export interface Props {
  keymap: KeymapState;
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

export const Component: React.FC<Props> = props => (
  <LocatorContext.Consumer>
    {({ context }) => (
      <Element.Component className={styles.root} onKeyDown={handleKeyDown(context, props.keymap)}>
        {props.children}
      </Element.Component>
    )}
  </LocatorContext.Consumer>
);
