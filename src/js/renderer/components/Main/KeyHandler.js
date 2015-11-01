import * as K from 'sxfiler/renderer/actions/Keyboard';
import {Pane} from 'sxfiler/common/Constants';
import path from 'path';

function getPaneInfo(state, pane) {
  switch (pane) {
  case Pane.LEFT:
    return {paneInfo:state.paneInfo.left, directory: state.directory.leftPane};
  case Pane.RIGHT:
    return {paneInfo:state.paneInfo.right, directory: state.directory.rightPane};
  default:
    throw new Error(`Unknown pane ${pane}`);
  }
}

function swapPane(pane) {
  return pane === Pane.LEFT ? Pane.RIGHT : Pane.LEFT;
}

export default class KeyHandler {
  /**
   * Handling keyboard event with component state.
   *
   * @param {string} key - Name of key what press/down/up
   * @param {object} state - State of component
   * @return {boolean} Return true if caller should prevent default.
   */
  static handleKeyEvents(key, state) {
    let {paneInfo, directory} = getPaneInfo(state, state.paneInfo.current);
    switch (key) {
    case 'j':
      if (paneInfo.position < Object.keys(directory.fileList).length - 1) {
        K.downCursor(state.paneInfo.current);
      }
      return false;
    case 'k':
      if (paneInfo.position >  0) {
        K.upCursor(state.paneInfo.current);
      }
      return false;
    case 'Tab':
      K.changePane(swapPane(state.paneInfo.current));
      return true;
    case 'Enter':
      (() => {
        let file = Object.keys(directory.fileList)[paneInfo.position];
        K.changeDirectory(path.join(directory.path, file), state.paneInfo.current);
      })();
      return false;
    default:
      return false;
    }
  }
}
