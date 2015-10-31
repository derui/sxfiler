import test from 'tape';
import * as D from 'sxfiler/renderer/actions/Directory';
import {Pane} from 'sxfiler/common/Constants';
import sinon from 'sinon';

/**
 * @test {Directory}
 */
let wrapper = (comment, f) => test(`renderer/actions/Directory ${comment}`, f);
let after = () => D.dispose();

wrapper('should be able to subscribes events', (st) => {
  let spy = sinon.spy();

  let dispose = D.subject.subscribe(spy);
  D.requestDirectory('left side', Pane.LEFT);
  D.receiveDirectory('path', [], Pane.RIGHT);

  st.ok(spy.calledWith({
    key: D.ACTIONS.REQUEST_DIRECTORY,
    path: 'left side',
    pane:Pane.LEFT
  }));
  st.ok(spy.calledWith({
    key: D.ACTIONS.RECEIVE_DIRECTORY,
    path: 'path',
    fileList: [],
    pane: Pane.RIGHT
  }));
  after();
  st.end();
});
