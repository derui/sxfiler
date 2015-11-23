import test from 'tape';
import Rx from 'rx';
import ActionStream from 'sxfiler/renderer/action-stream';
import createIpc from './createIpc';
import RendererIPC from 'sxfiler/renderer/renderer-ipc';

const wrapper = (label, f) => test(`renderer/action-stream ${label}`, f);

wrapper('can publish an action', (t) => {
  let s = new ActionStream(new RendererIPC(createIpc()));

  s.subscribe((v) => {
    t.deepEqual(v, {key: 'action'});
    t.end();
  });

  s.doAction(Rx.Observable.just({key: 'action'}));
});

wrapper('can publish an action that is needed with ipc', (t) => {
  let s = new ActionStream(new RendererIPC(createIpc()));

  s.subscribe((v) => {
    t.deepEqual(v, {key: 'action'});
    t.end();
  });

  s.doAction((ipc) => {
    t.notOk(!ipc, 'can get ipc');
    return Rx.Observable.just({key: 'action'});
  });
});
