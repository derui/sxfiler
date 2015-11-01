import React from 'react';
import ReactDOM from 'react-dom';
import Rx from 'rx';

import Main from './components/Main';

import RendererIPC from './RendererIPC';
import {Pane} from 'sxfiler/common/Constants';
import Store from './stores/Store';
import DirectoryBinder from './binders/Directory';
import KeyboardBinder from './binders/Keyboard';
import * as DirectoryAction from './actions/Directory';

let directory = new Store({
  leftPane: {
    path: '.',
    fileList: []
  },
  rightPane: {
    path: '.',
    fileList: []
  }
});

let paneInfo = new Store({
  current: Pane.LEFT,
  left: {
    position: 0
  },
  right: {
    position: 0
  }
});

let binder = new DirectoryBinder();
let actionBinder = new KeyboardBinder();

let state = Rx.Observable.combineLatest(
  directory.getSubject(),
  paneInfo.getSubject(),
  (directoryState, paneInfo) =>
    ({
      directory: directoryState,
      paneInfo
    })
);

state.subscribe((state) => {
  ReactDOM.render(React.createElement(Main, state), document.querySelector('.entry'));
});

let ipc = new RendererIPC(window.require('ipc'));
binder.bind(ipc, directory);
actionBinder.bind(ipc, paneInfo);

DirectoryAction.requestDirectory('.', Pane.LEFT);
DirectoryAction.requestDirectory('.', Pane.RIGHT);
