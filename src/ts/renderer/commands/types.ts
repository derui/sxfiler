import * as Rx from 'rx';

import {GlobalState} from 'sxfiler/renderer/types';

// a type of executable command
export type command = (globalState: GlobalState, actions: any) => Rx.Observable<any>;

// the Information for file
export interface Command {
  // the name of command
  name: string;

  // the identifier for command. this value is UUID
  id: string;

  // the command implemented function
  command: command;
}

export interface Commands {
  commands: Command[];
}
