import assert from 'power-assert';
import KeyHandler from 'sxfiler/renderer/components/Main/KeyHandler';
import * as K from 'sxfiler/renderer/actions/Keyboard';
import {Pane} from 'sxfiler/common/Constants';
import sinon from 'sinon';

/**
 * @test {Keyboard}
 */
describe('components/Main/KeyHandler', () => {
  after(() => K.dispose());
  it('should be able to execute cursor-down action', () => {
    let spy = sinon.spy();

    let dispose = K.subject.subscribe(spy);
    KeyHandler.handleKeyEvents('j', {
      paneInfo: {
        current: Pane.LEFT,
        cursorPosition: 0
      }
    });

    assert.ok(spy.calledWith({key: K.ACTIONS.DOWN_CURSOR, pane: Pane.LEFT}));
  });

  it('should be able to execute cursor-up action', () => {
    let spy = sinon.spy();

    let dispose = K.subject.subscribe(spy);
    KeyHandler.handleKeyEvents('k', {
      paneInfo: {
        current: Pane.LEFT,
        cursorPosition: 0
      }
    });

    assert.ok(spy.calledWith({key: K.ACTIONS.UP_CURSOR, pane: Pane.LEFT}));
  });

  it('should be able to execute pane-change action', () => {
    let spy = sinon.spy();

    let dispose = K.subject.subscribe(spy);
    KeyHandler.handleKeyEvents('Tab', {
      paneInfo: {
        current: Pane.LEFT,
        cursorPosition: 0
      }
    });

    assert.ok(spy.calledWith({key: K.ACTIONS.CHANGE_PANE, pane: Pane.LEFT}));
  });
});
