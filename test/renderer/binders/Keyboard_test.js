import test from 'tape';
import * as D from 'sxfiler/renderer/actions/Keyboard';
import {IPCKeys, Pane} from 'sxfiler/common/Constants';
import Store from 'sxfiler/renderer/stores/Store';
import Binder from 'sxfiler/renderer/binders/Keyboard';
import sinon from 'sinon';
import RendererIPC from 'sxfiler/renderer/RendererIPC';
import createIpc from '../createIpc';

/**
 * @test {Keyboard~binders}
 */
let wrapper = (comment, f) => test(`renderer/binders/Keyboard ${comment}`, f);
let after = () => D.dispose();
wrapper('should be able to bind keyboard actions to store', (t) => {
  let store = new Store({
    current: Pane.LEFT,
    left: {
      position: 0
    },
    right: {
      position: 0
    }
  });
  let binder = new Binder();
  binder.bind(new RendererIPC(createIpc()), store);

  D.downCursor(Pane.LEFT);
  D.upCursor(Pane.RIGHT);
  t.deepEqual(store.currentValue(), {
    current: Pane.LEFT,
    left: {
      position: 1
    },
    right: {
      position: -1
    }
  });
  after();
  t.end();
});

wrapper('should be able to notify to change pane', (t) => {
  let store = new Store({
    current: Pane.LEFT,
    left: {
      position: 0
    },
    right: {
      position: 0
    }
  });
  let binder = new Binder();
  binder.bind(new RendererIPC(createIpc()), store);

  D.changePane(Pane.RIGHT);
  t.equal(store.currentValue().current, Pane.RIGHT);
  after();
  t.end();
});

wrapper('should be able to notify to change directory', (t) => {
  let store = new Store({
    current: Pane.LEFT,
    left: {
      position: 0
    },
    right: {
      position: 0
    }
  });
  let binder = new Binder();
  let ipc = createIpc();
  let spy = sinon.spy();
  ipc.on(IPCKeys.REQUEST_FILES_IN_DIRECTORY, spy);
  binder.bind(new RendererIPC(ipc), store);

  D.changeDirectory('path', Pane.RIGHT);

  let called = spy.firstCall;
  t.equal(called.args[1], 'path');
  t.equal(called.args[2], Pane.RIGHT);
  after();
  t.end();
});

wrapper('should be able to send event to quit application', (t) => {
  let store = new Store();
  let binder = new Binder();
  let ipc = createIpc();
  let spy = sinon.spy();
  ipc.on(IPCKeys.QUIT_APPLICATION, spy);
  binder.bind(new RendererIPC(ipc), store);

  D.quitApplication();

  t.ok(spy.calledOnce, 'emitted event to quit');
  after();
  t.end();
});
