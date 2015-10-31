import test from 'tape';
import React from 'react';
import R from 'ramda';
import sinon from 'sinon';
import {Pane} from 'sxfiler/common/Constants';
import Main from 'sxfiler/renderer/components/Main';
import * as Keyboard from 'sxfiler/renderer/actions/Keyboard';
import FileList from 'sxfiler/renderer/components/FileList';
import TestUtils from 'react-addons-test-utils';

let c = React.createElement;

class Wrapper extends React.Component {
  render() {
    return c('div', {}, c(Main, R.merge({
      directory: {
        leftPane: {
          fileList: {
            'sample.txt' : {
              mode: 0o100644,
              size: 527
            },
            'test.js': {
              mode: 0o100644,
              size: 400
            }
          },
          path: 'left'
        },
        
        rightPane: {
          fileList: {
            'sample.txt' : {
              mode: 0o100644,
              size: 527
            },
            'test.js': {
              mode: 0o100644,
              size: 400
            }
          },
          path: 'right'
        }
      },
      paneInfo: {
        current: Pane.LEFT,
        left: {
          position: 0
        },
        right: {
          position: 0
        }
      }
    }, this.props)));
  }
}

let wrapper = (comment, f) => {
  test(`renderer/components/Main ${comment}`, f);
};

/** @test {Main} */
wrapper('should be instance Main', (t) => {
  t.ok(React.createElement(Main) !== null);
  t.end();
});

wrapper('should show two FileList as left and right', (t) => {
  let view = TestUtils.renderIntoDocument(c(Wrapper));
  view = TestUtils.scryRenderedComponentsWithType(view, FileList);
  let d = React.findDOMNode;

  t.equal(view.length, 2);
  t.end();
});

wrapper('should select either pane specified in props', (t) => {
  let view = TestUtils.renderIntoDocument(c(Wrapper));
  let main = TestUtils.findRenderedComponentWithType(view, Main);
  let d = React.findDOMNode;

  t.ok(main.refs.leftPane.props.current);
  t.ok(!main.refs.rightPane.props.current);
  t.end();
});

wrapper('can move cursor on current pane', (t) => {
  let view = TestUtils.renderIntoDocument(c(Wrapper));
  let main = TestUtils.findRenderedComponentWithType(view, Main);
  let d = React.findDOMNode;
  let spy = sinon.spy();
  Keyboard.subject.subscribe(spy);

  TestUtils.Simulate.keyPress(d(main), {key: 'j'});

  t.ok(spy.calledWith({
    key: Keyboard.ACTIONS.MOVE_CURSOR,
    pane: Pane.LEFT,
    direction: 1
  }), 'called down cursor action');

  Keyboard.dispose();

  t.end();
});
