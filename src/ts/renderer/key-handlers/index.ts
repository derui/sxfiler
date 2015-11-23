// provide key handler with key bindings
import * as Rx from 'rx';
import R from 'ramda';
import * as types from 'sxfiler/common/types';
import {GlobalState} from 'sxfiler/renderer/types';
import * as commands from 'sxfiler/renderer/commands';
import {getKeyIdentifier, bindMappingWithCommands} from 'sxfiler/renderer/utils/key-binding-util';
import defaultMapping from './defaults';
import * as actions from 'sxfiler/renderer/actions';
import * as Maybe from 'sxfiler/common/maybe';

let defaultCommands = commands.defaultCommands;

/**
 * Merge two keyDescriptors to one keyDescriptors.
 *
 * @param {array[KeyDescriptor]} src1 key bindings to merge with src2
 * @param {array[KeyDescriptor]} src2 key bindings to merge with src1
 * @return {array[KeyDescriptor]} merged key descriptors
 */
function mergeDescriptors(src1: types.KeyDescriptor[], src2: types.KeyDescriptor[]): types.KeyDescriptor[] {
  'use strict';

  let toObj: any = R.reduce((memo: any, v: types.KeyDescriptor) => R.merge(memo, {[getKeyIdentifier(v)]: v}), {});
  let src1Descs = toObj(src1), src2Descs = toObj(src2);

  return R.values(R.merge(src1Descs, src2Descs));
}

export interface KeyHandler {
  /**
   * Invoke a command binded a key-combination which calculated from KeyboardEvent.
   * @param {KeyDescriptor} desc - a descriptor to invoke command
   * @param {IGlobalState} globalState - the global state to pass command
   * @return {Rx.Observable} result of command
   */
  invokeCommand(desc: types.KeyDescriptor, globalState: GlobalState): Rx.Observable<any>;
}

/**
 * KeyHandler provide match keyboard input with keybindings that are defined user or default,
 *  and run command in it.
 */
export default class KeyHandlerImpl implements KeyHandler {
  private keyBindings: types.KeyBindings<commands.Command>;

  /**
   * Construct KeyHandler.
   *
   * @param {IKeyBindings} [keyBindings] key bindings to append to default binding
   */
  constructor(keyBindings?: types.KeyBindings<commands.Command>) {
    this.keyBindings = bindMappingWithCommands(defaultMapping, defaultCommands);

    // if keybindings given, merge with default bindings.
    if (keyBindings) {
      this.keyBindings.bindings = mergeDescriptors(this.keyBindings.bindings, keyBindings.bindings);
      this.keyBindings.mapping = R.merge(this.keyBindings.bindings, keyBindings.mapping);
    }
  }

  /**
   * Invoke a command binded a key-combination which calculated from KeyboardEvent.
   * @param {KeyDescriptor} desc - a descriptor to invoke command
   * @param {IGlobalState} globalState - the global state to pass command
   * @return {Rx.Observable} result of command
   */
  public invokeCommand(desc: types.KeyDescriptor, globalState: GlobalState): Rx.Observable<any> {

    let command = this.matchKey(desc);
    if (Maybe.isNone(command)) {
      return Rx.Observable.empty();
    }

    return command.val.command(globalState, actions);
  }

  /**
   * Match given KeyboardEvent with keybindings in this class
   *
   * @param {KeyDescriptor} desc - a descriptor to invoke command
   * @return {ICommand} a command if match keyboard event with keybindings. return null if not.
   */
  private matchKey(desc: types.KeyDescriptor): Maybe.Data<commands.Command> {
    if (!desc) {
      return Maybe.NONE;
    }

    let id = getKeyIdentifier(desc);

    let binding = this.keyBindings.mapping[id];
    if (!binding) {
      return Maybe.NONE;
    }
    return Maybe.just(binding);
  }
}
