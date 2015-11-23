/// <reference path="../../../typings/tsd.d.ts" />
import * as Rx from 'rx';

import * as ReactDOM from 'react-dom';
import * as React from 'react';
import uuid from 'uuid';
import R from 'ramda';

import Main from './components/main';
import ActionStream from './action-stream';
import RendererIPC from './renderer-ipc';
import {PANE} from 'sxfiler/common/constants';
import Directory from './binders/directory';
import CurrentPane from './binders/current-pane';
import * as types from 'sxfiler/renderer/types';
import * as actions from './actions';
import KeyHandler from './key-handlers';
import {convertEventToDescriptor} from 'sxfiler/renderer/utils/key-binding-util';

let ipc = new RendererIPC(window.require('electron').ipcRenderer);
let actionStream = new ActionStream(ipc);

let currentPane = new CurrentPane(PANE.LEFT, actionStream);
let leftPane = new Directory(PANE.LEFT, actionStream);
let rightPane = new Directory(PANE.RIGHT, actionStream);
let paneInfo = Rx.Observable.combineLatest(
  leftPane.store.getObservable(),
  rightPane.store.getObservable(),
  (left: types.PaneInfo, right: types.PaneInfo): types.PaneInfos => {
    return {
      [PANE.LEFT]: left,
      [PANE.RIGHT]: right
    };
  });

let globalState = Rx.Observable.combineLatest(
  paneInfo,
  currentPane.store.getObservable(),
  (paneInfo: types.PaneInfos, currentPane: string): types.GlobalState => {
    return {
      paneInfo,
      currentPane
    };
  }).publish();

globalState.connect();

globalState.subscribe((state: types.GlobalState) => {
  ReactDOM.render(React.createElement(Main, {state, actionStream}), document.querySelector('.entry'));
});

// setup keyevent stream
interface StateWithEvent {
  event: KeyboardEvent;
  eventId: string;
  state: types.GlobalState;
};

let keyEvents = Rx.Observable.combineLatest(
  Rx.Observable.fromEvent(document.body, 'keydown')
    .select((e: KeyboardEvent) => {
      return {
        event: e,
        id: uuid.v4()
      };
    }),
  globalState,
  (ev: {event: KeyboardEvent, id: string}, state: types.GlobalState): StateWithEvent => {
    return {
      event: ev.event,
      eventId: ev.id,
      state
    };
  });

// this variable only store event id from observable sequence as `keyEvents'
let _lastEventId = '';
let keyHandler = new KeyHandler();
keyEvents.subscribe((state: StateWithEvent) => {
  if (state.eventId === _lastEventId) {
    return;
  }

  _lastEventId = state.eventId;

  let _e = R.clone(state.event);
  let desc = convertEventToDescriptor(_e);
  actionStream.doAction(
    keyHandler.invokeCommand(desc, state.state)
  );
});

actionStream.doAction(actions.directory.getDirectory('.', PANE.LEFT));
actionStream.doAction(actions.directory.getDirectory('.', PANE.RIGHT));
