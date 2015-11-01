import test from 'tape';
import Store from 'sxfiler/renderer/stores/Store';
import Rx from 'rx';
import sinon from 'sinon';

/** @test {Store} */
let wrapper = (comment, f) => test(`renderer/stores/Store ${comment}`, f);
wrapper("can initialize with constructor value", (st) => {
  st.plan(2);
  let store = new Store(100);

  store.getSubject().subscribe((v) => {
    st.equal(v, 100);
  });
  st.equal(store.currentValue(), 100);
});

wrapper("should update immediately publish each subscriptions", (st) => {
  st.plan(4);
  let store = new Store(100);
  store.getSubject().subscribe((v) => {
    st.ok(v === 10 || v === 100);
  });
  store.getSubject().subscribe((v) => {
    st.ok(v === 10 || v === 100);
  });

  store.update(function() {return 10;});
});

wrapper("can apply operations to update value", (t) => {
  let store = new Store(100);
  store.update(function(x) {
    return x * 2;
  });

  store.update(function(x) {
    return x * 2;
  });

  t.equal(store.currentValue(), 400);
  t.end();
});
