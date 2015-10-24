import * as K from 'sxfiler/renderer/actions/Keyboard';

export default class KeyHandler {
  /**
   * Handling keyboard event with component state.
   *
   * @param {string} key - Name of key what press/down/up
   * @param {object} state - State of component
   */
  static handleKeyEvents(key, state) {
    switch (key) {
    case 'j':
      K.downCursor(state.paneInfo.current);
      break;
    case 'k':
      K.upCursor(state.paneInfo.current);
      break;
    case 'Tab':
      K.changePane(state.paneInfo.current);
      break;
    default:
      break;
    }
  }
}
