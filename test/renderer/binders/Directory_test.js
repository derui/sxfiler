import assert from 'power-assert';
import * as D from 'sxfiler/renderer/actions/Directory';
import {Pane} from 'sxfiler/common/Constants';
import Store from 'sxfiler/renderer/stores/Store';
import Binder from 'sxfiler/renderer/binders/Directory';
import sinon from 'sinon';

/**
 * @test {Directory~binders}
 */
describe('binders/Directory', () => {
  afterEach(() => D.dispose());

  it('should be able to bind directory actions', () => {
    let spy = sinon.spy();
    let store = new Store({
      leftPane: {},
      rightPane: {}
    });
    let binder = new Binder();
    binder.bind(store);

    store.subscribe(spy);
    D.requestDirectory('left side', Pane.LEFT);

    assert.ok(spy.calledWith({
      leftPane: {path: 'left side'},
      rightPane: {}
    }));
  });

  it('can unsubscribe all binding observer from action', () => {
    let store = new Store({
      leftPane: {},
      rightPane: {}
    });
    let binder = new Binder();
    binder.bind(store);

    D.requestDirectory('left side', Pane.LEFT);

    binder.dispose();

    // ignore published event after disposed.
    D.requestDirectory('right side', Pane.RIGHT);

    assert.deepEqual(store.currentValue(), {
      leftPane: {path: 'left side'},
      rightPane: {}
    });
  });
});
