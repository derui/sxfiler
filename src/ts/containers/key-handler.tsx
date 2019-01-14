import * as React from "react";

import * as kbd from "../kbd";
import {KeymapState} from "../types/store-state";

type ActionCallback = (action: string) => void

interface Prop {
  className: string;
  keymap: KeymapState;
  onAction: ActionCallback;
}

function findBinding(keymap: KeymapState, key: string) {
  return keymap.current.find(key);
}

/**
 * handle keyboard event that all keydown event on application
 * @param props properties of component
 */
function handleKeyDown(props: Prop) : (ev:React.KeyboardEvent<any>) => void {
  return ev => {
    switch (ev.type) {
      case "keydown": {
        const key = kbd.make(ev.key, {meta: ev.metaKey, ctrl: ev.ctrlKey});
        const binding = findBinding(props.keymap, kbd.toKeySeq(key));

        if (binding === null) {
          break ;
        }

        ev.preventDefault();
        ev.stopPropagation();
        props.onAction(binding.action);
        break;
      };
      default: break;
    }

    return;
  };
}

const KeyHandler : React.FC<Prop> = props => {
  return (
    <div className={props.className} onKeyDown={handleKeyDown(props)}>
      {props.children}
    </div>
  );
};

export default KeyHandler;
