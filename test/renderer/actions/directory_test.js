import {IPC_KEYS} from 'sxfiler/common/constants';
import test from 'tape';
import * as D from 'sxfiler/renderer/actions/directory';
import sinon from 'sinon';
import createIpc from '../createIpc';
import RendererIPC from 'sxfiler/renderer/renderer-ipc';

/**
 * @test {Directory}
 */
let wrapper = (comment, f) => test(`renderer/actions/Directory ${comment}`, f);

wrapper('should be able to subscribes events', (st) => {
  let ipc = createIpc();
  let main = new RendererIPC(ipc);
  let spy = sinon.spy();

  ipc.on(IPC_KEYS.REQUEST_FILES_IN_DIRECTORY, (ev, path, ...args) => {
    ev.sender.send(IPC_KEYS.FINISH_FILES_IN_DIRECTORY, null, path, [], ...args);
  });

  D.getDirectory('left side', 'pane')(main).subscribe(spy);

  st.ok(spy.calledTwice);
  st.ok(spy.calledWith({
    key: D.ACTIONS.REQUEST_DIRECTORY,
    pane: 'pane',
    path: 'left side'
  }));

  st.ok(spy.calledWith({
    key: D.ACTIONS.RECEIVE_DIRECTORY,
    path: 'left side',
    pane: 'pane',
    fileList: []
  }));
  st.end();
});

wrapper('should be able to handling error from ipc', (st) => {
  let ipc = createIpc();
  let main = new RendererIPC(ipc);
  let spy = sinon.spy();

  ipc.on(IPC_KEYS.REQUEST_FILES_IN_DIRECTORY, (ev, path, ...args) => {
    ev.sender.send(IPC_KEYS.FINISH_FILES_IN_DIRECTORY, 'error', path, [], ...args);
  });

  D.getDirectory('left side', 'pane')(main).subscribe(spy, spy);

  st.ok(spy.calledTwice);
  st.ok(spy.calledWith({
    key: D.ACTIONS.REQUEST_DIRECTORY,
    pane: 'pane',
    path: 'left side'
  }));

  st.ok(spy.calledWith(new Error('error')));
  st.end();
});
