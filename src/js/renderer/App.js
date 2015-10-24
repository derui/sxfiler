import React from 'react';
import ReactDOM from 'react-dom';

import Main from './components/Main';

import RendererIPC from './RendererIPC';
import {IPCKeys, Pane} from 'sxfiler/common/Constants';
import Store from './stores/Store';
import DirectoryBinder from './binders/Directory';
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

let binder = new DirectoryBinder();

binder.bind(directory);

directory.subscribe((state) => {
  ReactDOM.render(React.createElement(Main, state), document.querySelector('.entry'));
});

let ipc = new RendererIPC(window.require('ipc'));
ipc.subscribe(IPCKeys.FINISH_FILES_IN_DIRECTORY, ([err, files, pane]) => {
  if (err) {
    return;
  }

  DirectoryAction.receiveDirectory(files, pane);
});

ipc.send(IPCKeys.REQUEST_FILES_IN_DIRECTORY, '.', Pane.LEFT);
ipc.send(IPCKeys.REQUEST_FILES_IN_DIRECTORY, '.', Pane.RIGHT);
