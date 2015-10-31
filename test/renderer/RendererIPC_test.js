import test from 'tape';
import RendererIPC from 'sxfiler/renderer/RendererIPC';
import {IPCKeys} from 'sxfiler/common/Constants';
import sinon from 'sinon';
import Rx from 'rx';
import createIpc from './createIpc';

/**
 * @test {RendererIPC}
 */
test('renderer/RendererIPC', (t) => {
  t.test('should be able to instance', (st) => {
    st.ok(new RendererIPC(createIpc()) instanceof RendererIPC);
    st.end();
  });

  t.test('should be able to send and subscribe message', (st) => {
    st.plan(2);
    let ipc = createIpc();
    let main = new RendererIPC(ipc);
    main.subscribe(IPCKeys.FINISH_FILES_IN_DIRECTORY, ([err, message]) => {
      st.equal(err, null);
      st.equal(message, "message");
    });

    ipc.on(IPCKeys.REQUEST_FILES_IN_DIRECTORY, (ev, ...args) => {
      ev.sender.send(IPCKeys.FINISH_FILES_IN_DIRECTORY, ...args);
    });
    main.send(IPCKeys.REQUEST_FILES_IN_DIRECTORY, null, "message");
  });

  t.test('should be able to dispose description', (st) => {
    st.plan(1);
    let ipc = createIpc();
    let main = new RendererIPC(ipc);
    let dispose = main.subscribe(IPCKeys.FINISH_FILES_IN_DIRECTORY, () => {
      st.pass("caused publish");
    });
    ipc.on(IPCKeys.REQUEST_FILES_IN_DIRECTORY, (ev, ...args) => {
      ev.sender.send(IPCKeys.FINISH_FILES_IN_DIRECTORY, ...args);
    });
    main.send(IPCKeys.REQUEST_FILES_IN_DIRECTORY, null, "message", "PANE");

    dispose.dispose();
    main.send(IPCKeys.REQUEST_FILES_IN_DIRECTORY, null, "message2", 'PANE');
  });
  t.end();
});
