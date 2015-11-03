import test from 'tape';
import KeyHandler from 'sxfiler/renderer/components/Main/KeyHandler';
import * as K from 'sxfiler/renderer/actions/Keyboard';
import {Pane} from 'sxfiler/common/Constants';
import sinon from 'sinon';
import path from 'path';

/**
 * @test {KeyHandler}
 */
let wrapper = (comment, f) => test(`components/Main/KeyHandler ${comment}`, f);
let after = () => K.dispose();
wrapper('should be able to execute cursor-down action', (st) => {
  let spy = sinon.spy();

  let dispose = K.subject.subscribe(spy);
  KeyHandler.handleKeyEvents('j', {
    directory: {
      leftPane: {
        fileList: [1, 22]
      }
    },
    paneInfo: {
      current: Pane.LEFT,
      left: {
        position: 0
      }
    }
  });

  st.ok(spy.calledWith({
    key: K.ACTIONS.MOVE_CURSOR, pane: Pane.LEFT,
    direction: 1
  }));
  after();

  st.end();
});

wrapper('should be able to execute cursor-up action', (st) => {
  let spy = sinon.spy();

  let dispose = K.subject.subscribe(spy);
  KeyHandler.handleKeyEvents('k', {
    directory: {
      leftPane: {
        fileList: [1]
      }
    },
    paneInfo: {
      current: Pane.LEFT,
      left: {
        position: 1
      }
    }
  });

  st.ok(spy.calledWith({key: K.ACTIONS.MOVE_CURSOR, pane: Pane.LEFT, direction: -1}));
  after();
  st.end();
});

wrapper('should be able to execute pane-change action', (st) => {
  let spy = sinon.spy();

  let dispose = K.subject.subscribe(spy);
  KeyHandler.handleKeyEvents('Tab', {
    paneInfo: {
      current: Pane.LEFT
    },
    directory: {

    }
  });

  st.ok(spy.calledWith({key: K.ACTIONS.CHANGE_PANE, pane: Pane.RIGHT}));
  after();
  st.end();
});

wrapper('should be able to execute change-directory action', (st) => {
  let spy = sinon.spy();

  let dispose = K.subject.subscribe(spy);
  KeyHandler.handleKeyEvents('Enter', {
    directory: {
      leftPane: {
        path: 'sample',
        fileList: {'path':{}}
      }
    },
    paneInfo: {
      current: Pane.LEFT,
      left: {
        position: 0
      }
    }
  });

  let called = spy.firstCall;
  st.deepEqual(called.args[0], {
    key: K.ACTIONS.CHANGE_DIRECTORY, pane: Pane.LEFT,
    path: path.join('sample', 'path')
  });
  after();
  st.end();
});

wrapper('should be able to execute to quit application', (st) => {
  let spy = sinon.spy();

  let dispose = K.subject.subscribe(spy);
  KeyHandler.handleKeyEvents('q', {
    directory: {
      leftPane: {
        path: 'sample',
        fileList: {'path':{}}
      }
    },
    paneInfo: {
      current: Pane.LEFT,
      left: {
        position: 0
      }
    }
  });

  let called = spy.firstCall;
  st.deepEqual(called.args[0], {key: K.ACTIONS.QUIT_APPLICATION});
  after();
  st.end();
});


wrapper('should be able to go upward directory path', (st) => {
  let spy = sinon.spy();

  let dispose = K.subject.subscribe(spy);
  KeyHandler.handleKeyEvents('Backspace', {
    directory: {
      leftPane: {
        path: '/test/sample',
        fileList: {'path':{}}
      }
    },
    paneInfo: {
      current: Pane.LEFT,
      left: {
        position: 0
      }
    }
  });

  let called = spy.firstCall;
  st.deepEqual(called.args[0], {
    key: K.ACTIONS.CHANGE_DIRECTORY,
    path: '/test',
    pane: Pane.LEFT
  });
  after();
  st.end();
});

wrapper('should not go upward from root directory', (st) => {
  let spy = sinon.spy();

  let dispose = K.subject.subscribe(spy);
  KeyHandler.handleKeyEvents('Backspace', {
    directory: {
      leftPane: {
        path: '/',
        fileList: {'path':{}}
      }
    },
    paneInfo: {
      current: Pane.LEFT,
      left: {
        position: 0
      }
    }
  });

  let called = spy.firstCall;
  st.deepEqual(called.args[0], {
    key: K.ACTIONS.CHANGE_DIRECTORY,
    path: '/',
    pane: Pane.LEFT
  });
  after();
  st.end();
});
