import test from 'tape';
import RendererIPC from 'sxfiler/renderer/renderer-ipc';
import {IPC_KEYS} from 'sxfiler/common/constants';
import sinon from 'sinon';
import createIpc from './createIpc';

let wrapper = (comment, f) => test(`renderer/RendererIPC ${comment}`, f);

/**
 * @test {RendererIPC}
 */
wrapper('should be able to instance', (st) => {
  st.ok(new RendererIPC(createIpc()) instanceof RendererIPC);
  st.end();
});

wrapper('should be able to send and subscribe message', (st) => {
  st.plan(4);
  let ipc = createIpc();
  let main = new RendererIPC(ipc);
  main.subscribe(IPC_KEYS.REQUEST_FILES_IN_DIRECTORY, ([err, message]) => {
    st.equal(err, null);
    st.equal(message, 'message');
  });

  ipc.on(IPC_KEYS.REQUEST_FILES_IN_DIRECTORY, (ev, ...args) => {
    ev.sender.send(IPC_KEYS.FINISH_FILES_IN_DIRECTORY, ...args);
  });

  main.send(IPC_KEYS.REQUEST_FILES_IN_DIRECTORY, null, 'message').subscribe(([err, message]) => {
    st.equal(err, null);
    st.equal(message, 'message');
  });
});

wrapper('should be able to dispose description', (st) => {
  st.plan(1);
  let ipc = createIpc();
  let main = new RendererIPC(ipc);
  let dispose = main.subscribe(IPC_KEYS.REQUEST_FILES_IN_DIRECTORY, () => {
    st.pass('caused publish');
  });
  ipc.on(IPC_KEYS.REQUEST_FILES_IN_DIRECTORY, (ev, ...args) => {
    ev.sender.send(IPC_KEYS.FINISH_FILES_IN_DIRECTORY, ...args);
  });
  main.send(IPC_KEYS.REQUEST_FILES_IN_DIRECTORY, null, 'message', 'PANE');

  dispose.dispose();
  main.send(IPC_KEYS.REQUEST_FILES_IN_DIRECTORY, null, 'message2', 'PANE');
});

wrapper('can send message to main process to quit application', (st) => {
  let ipc = createIpc();
  let main = new RendererIPC(ipc);

  ipc.on(IPC_KEYS.QUIT_APPLICATION, () => {
    st.pass('called quit application');
    st.end();
  });
  main.send(IPC_KEYS.QUIT_APPLICATION);
});

wrapper('should be able to get only first value returning observeable', (st) => {
  let ipc = createIpc();
  let main = new RendererIPC(ipc);
  let spy = sinon.spy(), spy2 = sinon.spy();
  ipc.on(IPC_KEYS.REQUEST_FILES_IN_DIRECTORY, (ev, ...args) => {
    ev.sender.send(IPC_KEYS.FINISH_FILES_IN_DIRECTORY, ...args);
  });
  main.send(IPC_KEYS.REQUEST_FILES_IN_DIRECTORY, null, 'message', 'PANE').subscribe(spy);
  main.send(IPC_KEYS.REQUEST_FILES_IN_DIRECTORY, null, 'message2', 'PANE2').subscribe(spy2);

  st.ok(spy.calledOnce, 'called');
  st.ok(spy2.calledOnce, 'called');
  st.deepEqual(spy.getCall(0).args[0].slice(0, 3), [null, 'message', 'PANE']);
  st.deepEqual(spy2.getCall(0).args[0].slice(0, 3), [null, 'message2', 'PANE2']);
  st.end();
});
