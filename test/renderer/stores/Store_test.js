import assert from 'power-assert';
import Store from 'sxfiler/renderer/stores/Store';
import Rx from 'rx';
import sinon from 'sinon';

/** @test {Store} */
describe("renderer/stores/Store", () => {
  it("can initialize with constructor value", (cb) => {
    let store = new Store(100);

    store.subscribe((v) => {
      assert.equal(v, 100);
      cb();
    });
  });

  it("can update new value into it", (cb) => {
    let store = new Store(100);

    store.update(10);

    store.subscribe((v) => {
      assert.equal(v, 10);
      cb();
    });
  });

  it("should update immediately publish each subscriptions", () => {
    let store = new Store(100);
    let spy = sinon.spy();
    let dispose = store.subscribe(spy);

    store.update(10);

    dispose.dispose();

    store.update(20);

    assert.ok(spy.calledTwice);
    assert.ok(spy.calledWith(100));
    assert.ok(spy.calledWith(10));
  });

});
