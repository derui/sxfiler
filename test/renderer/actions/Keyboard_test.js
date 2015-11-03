import test from 'tape';
import * as K from 'sxfiler/renderer/actions/Keyboard';
import {Pane} from 'sxfiler/common/Constants';
import sinon from 'sinon';

/**
 * @test {Keyboard}
 */
let wrapper = (comment, f) => test(`actions/Keyboard ${comment}`, f);
let after = () => K.dispose();

wrapper('should be able to subscribes events', (t) => {
  let spy = sinon.spy();

  let dispose = K.subject.subscribe(spy);
  K.upCursor(Pane.LEFT);
  K.downCursor(Pane.RIGHT);
  K.changePane(Pane.LEFT);

  t.ok(spy.calledWith({
    key: K.ACTIONS.MOVE_CURSOR,
    pane:Pane.LEFT,
    direction: -1
  }));
  t.ok(spy.calledWith({
    key: K.ACTIONS.MOVE_CURSOR,
    pane: Pane.RIGHT,
    direction: 1
  }));
  t.ok(spy.calledWith({
    key: K.ACTIONS.CHANGE_PANE,
    pane: Pane.LEFT
  }));

  after();

  t.end();
});

wrapper('should be able to enter into directory', (t) => {
  let spy = sinon.spy();

  let dispose = K.subject.subscribe(spy);
  K.changeDirectory('path', Pane.LEFT);

  t.ok(spy.calledWith({
    key: K.ACTIONS.CHANGE_DIRECTORY,
    path: 'path',
    pane:Pane.LEFT
  }));
  after();

  t.end();
});

wrapper('should be able to quit application', (t) => {
  let spy = sinon.spy();

  let dispose = K.subject.subscribe(spy);
  K.quitApplication();

  t.ok(spy.calledWith({
    key: K.ACTIONS.QUIT_APPLICATION
  }));
  after();

  t.end();
});
