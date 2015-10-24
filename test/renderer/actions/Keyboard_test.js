import assert from 'power-assert';
import * as K from 'sxfiler/renderer/actions/Keyboard';
import {Pane} from 'sxfiler/common/Constants';
import sinon from 'sinon';

/**
 * @test {Keyboard}
 */
describe('actions/Keyboard', () => {
  after(() => K.dispose());
  it('should be able to subscribes events', () => {
    let spy = sinon.spy();

    let dispose = K.subject.subscribe(spy);
    K.upCursor(Pane.LEFT);
    K.downCursor(Pane.RIGHT);
    K.changePane(Pane.LEFT);

    assert.ok(spy.calledWith({
      key: K.ACTIONS.UP_CURSOR,
      pane:Pane.LEFT
    }));
    assert.ok(spy.calledWith({
      key: K.ACTIONS.DOWN_CURSOR,
      pane: Pane.RIGHT
    }));
    assert.ok(spy.calledWith({
      key: K.ACTIONS.CHANGE_PANE,
      pane: Pane.LEFT
    }));
  });

});
