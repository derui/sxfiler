import {IPC_KEYS} from 'sxfiler/common/constants';
import test from 'tape';
import * as W from 'sxfiler/renderer/actions/window';
import sinon from 'sinon';
import createIpc from '../createIpc';
import RendererIPC from 'sxfiler/renderer/renderer-ipc';

/**
 * @test {Directory}
 */
let wrapper = (comment, f) => test(`renderer/actions/Windows ${comment}`, f);

wrapper('should be able to subscribes events', (st) => {
  let ipc = createIpc();
  let main = new RendererIPC(ipc);
  let spy = sinon.spy();

  ipc.on(IPC_KEYS.QUIT_APPLICATION, spy);

  W.quitApplication()(main);

  st.ok(spy.calledOnce, 'only once calling for quit application');
  st.end();
});

