import test from 'tape';
import * as D from 'sxfiler/renderer/actions/Directory';
import {IPCKeys, Pane} from 'sxfiler/common/Constants';
import Store from 'sxfiler/renderer/stores/Store';
import Binder from 'sxfiler/renderer/binders/Directory';
import sinon from 'sinon';
import RendererIPC from 'sxfiler/renderer/RendererIPC';
import createIpc from '../createIpc';

/**
 * @test {Directory~binders}
 */
let wrapper = (comment, f) => test(`renderer/binders/Directory ${comment}`, f);
let after = () => D.dispose();
wrapper('should be able to bind directory actions', (t) => {
  t.plan(2);
  let spy = sinon.spy();
  let values = [
    {
      leftPane: {},
      rightPane: {}
    }, {
      leftPane: {path: 'left side'},
      rightPane: {}
    }
  ];
  let store = new Store(values[0]);
  let ipc = new RendererIPC(createIpc());
  let binder = new Binder();
  let count = 0;
  binder.bind(ipc, store);

  store.getSubject().subscribe((v) => {
    t.deepEqual(v, values[count]);
    count = count + 1;
  });
  D.requestDirectory('left side', Pane.LEFT);
  after();
});

wrapper('can get current value unless dispose', (t) => {
  let store = new Store({
    leftPane: {},
    rightPane: {}
  });
  let ipc = new RendererIPC(createIpc());
  let binder = new Binder();
  binder.bind(ipc, store);

  D.requestDirectory('right side', Pane.RIGHT);

  binder.dispose();

  // ignore published event after disposed.
  D.requestDirectory('left side', Pane.LEFT);

  t.deepEqual(store.currentValue(), {
    leftPane: {},
    rightPane: {path: 'right side'}
  });
  after();
  t.end();
});

wrapper('can not subscribe after dispose', (t) => {
  let store = new Store({
    leftPane: {},
    rightPane: {}
  });

  let ipc = new RendererIPC(createIpc());
  let binder = new Binder();
  binder.bind(ipc, store);
  binder.dispose();

  D.requestDirectory('left pane', Pane.LEFT);
  t.deepEqual(store.currentValue(), {
    leftPane: {},
    rightPane: {}
  });
  after();
  t.end();
});

wrapper('should handle event when finish file list traversing', (t) => {
  t.plan(3);
  let store = new Store({
    leftPane: {},
    rightPane: {}
  });

  let origin = createIpc();
  let ipc = new RendererIPC(origin);
  let binder = new Binder();

  origin.on(IPCKeys.REQUEST_FILES_IN_DIRECTORY, (ev, path, pane) => {
    t.equal(path, 'left pane', `request path is ${path}`);
    t.equal(pane, Pane.LEFT, `request pane is ${pane}`);
    ev.sender.send(IPCKeys.FINISH_FILES_IN_DIRECTORY, null, path, [{[path]: {}}], pane);
  });

  binder.bind(ipc, store);

  D.requestDirectory('left pane', Pane.LEFT);

  t.deepEqual(store.currentValue(), {
    leftPane: {
      path: 'left pane',
      fileList: [{
        'left pane': {}
      }]
    },
    rightPane: {}
  }, 'merged request and receive event');
  after();
});
