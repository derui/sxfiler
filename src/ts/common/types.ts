import * as fs from 'fs';

import {KeyDescriptor} from 'sxfiler-kbd';

export {KeyDescriptor} from 'sxfiler-kbd';

// the information for file
export interface File {
  filename: string;
  id: number;
  stat: fs.Stats;
}

export type KeyIdentifier = string;

// define mapping key to generics
export interface KeyMapping<T> {
  [key: string]: T;
}

// an interface for key bindings in handler
export interface KeyBindings<T> {
  bindings: KeyDescriptor[];
  mapping: KeyMapping<T>;
}
