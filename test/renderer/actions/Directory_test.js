import assert from 'power-assert';
import * as D from 'sxfiler/renderer/actions/Directory';
import {Pane} from 'sxfiler/common/Constants';
import sinon from 'sinon';

/**
 * @test {Directory}
 */
describe('actions/Directory', () => {
  after(() => D.dispose());
  it('should be able to subscribes events', () => {
    let spy = sinon.spy();

    let dispose = D.subject.subscribe(spy);
    D.requestDirectory('left side', Pane.LEFT);
    D.receiveDirectory([], Pane.RIGHT);

    assert.ok(spy.calledWith({
      key: D.ACTIONS.REQUEST_DIRECTORY,
      path: 'left side',
      pane:Pane.LEFT
    }));
    assert.ok(spy.calledWith({
      key: D.ACTIONS.RECEIVE_DIRECTORY,
      fileList: [],
      pane: Pane.RIGHT
    }));
  });

});
