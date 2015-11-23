import * as Directory from './defaults/directory';
import * as Pane from './defaults/pane';
import * as Window from './defaults/window';

// export default commands.
import * as types from './types';

// exporting all types from current module.
export * from './types';

export const defaultCommands: types.Commands = {
  commands: [
    Directory.upDirectory,
    Directory.downDirectory,
    Pane.upCursor,
    Pane.downCursor,
    Pane.changePane,
    Window.quitApplication
  ]
};
