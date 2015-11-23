import test from 'tape';
import sinon from 'sinon';
import KeyHandler from 'sxfiler/renderer/key-handlers';

const wrapper = (label, f) => test(`renderer/key-handlers ${label}`, f);

wrapper('KeyHandler can invoke command that is matched descriptor', (t) => {
  let spy = sinon.spy();
  let kh = new KeyHandler({
    bindings: [],
    mapping: {
      'j': {
        command: spy
      }
    }
  });

  kh.invokeCommand({key: 'j'}, {});
  t.ok(spy.calledOnce, 'command invoked');
  t.deepEqual(spy.getCall(0).args[0], {}, 'called with empty state and actions');
  t.end();
});

wrapper('KeyHandler should not invoke command with not contained descriptor', (t) => {
  let spy = sinon.spy();
  let kh = new KeyHandler({
    bindings: [],
    mapping: {
      'j': {
        command: spy
      }
    }
  });

  kh.invokeCommand({key: 'a'}, {});
  t.notOk(spy.called, 'command was not invoked');
  t.end();
});
