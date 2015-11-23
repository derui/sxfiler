import * as types from 'sxfiler/common/types';
import * as commands from 'sxfiler/renderer/commands';
import R from 'ramda';
import kbd from 'sxfiler-kbd';

/**
 * Get KeyIdentifier of the binding.
 * 
 * @param {KeyDescriptor} binding a binding is based to generate identifier
 * @return {KeyIdentifier} an identifier of the binding
 */
export function getKeyIdentifier(binding: types.KeyDescriptor): types.KeyIdentifier {
  'use strict';
  if (!binding) {
    return null;
  }

  let {key, ctrlKey, altKey, metaKey, shiftKey} = binding;
  let conv: (b: boolean, pref: string) => string = (b, pref) => b ? `${pref}-` : '';

  return `${conv(ctrlKey, 'C')}${conv(shiftKey, 'S')}${conv(altKey, 'A')}${conv(metaKey, 'M')}${key}`;
}

/**
 * Convert KeyboardEvent to key descriptor.
 *
 * @param {KeyboardEvent} e a keyboard event
 * @return {IkeyDescriptor} a key descriptor converted from e
 */
export function convertEventToDescriptor(e: KeyboardEvent): types.KeyDescriptor {
  'use strict';
  if (!e || typeof e !== 'object') {
    return null;
  }

  let {ctrlKey, shiftKey, altKey, metaKey, which} = e;
  return {
    key: which,
    ctrlKey: ctrlKey || false,
    shiftKey: shiftKey || false,
    altKey: altKey || false,
    metaKey: metaKey || false
  };
}

/**
 * Return KeyBindings that binded key mappings with commands.
 *
 * @param {object} mapping a key mappings that have key which is a combination of key, and value is name of command
 * @param {ICommands} commands list of commands
 * @return {IKeyBindings<ICommand>} key bindings for invoking command
 */
export function bindMappingWithCommands(mapping: any, commands: commands.Commands)
: types.KeyBindings<commands.Command> {
  'use strict';

  let findCommand = (name: string) => R.find((com: commands.Command) => com.name === name, commands.commands);

  let keyBindings = R.pipe(R.toPairs, R.map((val: string[]) => {
    let [key, com] = val;
    return [kbd(key)[0], findCommand(com)];
  }))(mapping);

  let keyDescriptors = R.map(R.head)(keyBindings);

  let keyMap = R.reduce((memo: any, val: any[]) => {
    let [id, com] = val;
    if (!id) { return memo; }

    return R.merge(memo, {[getKeyIdentifier(id)]: com});
  },
                        {}, keyBindings);

  return {
    bindings: keyDescriptors,
    mapping: keyMap
  };
}
