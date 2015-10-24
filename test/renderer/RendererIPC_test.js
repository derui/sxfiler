import assert from 'power-assert';
import RendererIPC from 'sxfiler/renderer/RendererIPC';
import {IPCKeys} from 'sxfiler/common/Constants';
import Stub from './IpcStub';
import sinon from 'sinon';
import Rx from 'rx';

/**
 * @test {RendererIPC}
 */
describe('RendererIPC', () => {
  it('should be able to instance', () => {
    assert.ok(new RendererIPC(Stub()) instanceof RendererIPC);
  });

  it('should be able to send and subscribe message', (cb) => {
    let ipc = new RendererIPC(Stub());
    ipc.subscribe(IPCKeys.FINISH_FILES_IN_DIRECTORY, ([err, message]) => {

      assert.equal(err, null);
      assert.equal(message, "message");
      cb();
    });
    ipc.send(IPCKeys.FINISH_FILES_IN_DIRECTORY, null, "message");
  });

  it('should be able to dispose description', () => {
    let ipc = new RendererIPC(Stub());
    let onNext = sinon.spy();
    let obs = Rx.Observer.create(onNext);
    let dispose = ipc.subscribe(IPCKeys.FINISH_FILES_IN_DIRECTORY, obs);
    ipc.send(IPCKeys.FINISH_FILES_IN_DIRECTORY, null, "message", 'PANE');

    dispose.dispose();
    ipc.send(IPCKeys.FINISH_FILES_IN_DIRECTORY, null, "message2", 'PANE');

    assert.ok(onNext.calledOnce, 'onNext only called once');
    assert.ok(onNext.calledWith([null, "message", "PANE"]), 'onNext only called with these arguments');
  });
});
