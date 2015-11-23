import test from 'tape';
import React from 'react';
import R from 'ramda';
import {PANE} from 'sxfiler/common/constants';
import Main from 'sxfiler/renderer/components/main';
import FileList from 'sxfiler/renderer/components/file-list';
import TestUtils from 'react-addons-test-utils';
import {setup, cleanup} from '../jsdom-setup';

let c = React.createElement;

class Wrapper extends React.Component {
  render() {
    return c('div', {}, c(Main, R.merge({
      state: {
        currentPane: PANE.LEFT,
        paneInfo: {
          [PANE.LEFT]: {
            fileList: [
              {
                filename: 'sample.txt',
                id: 1,
                stat: {
                  mode: 0o100644,
                  size: 527
                }
              },
              {
                filename: 'test.js',
                id: 2,
                stat: {
                  mode: 0o100644,
                  size: 400
                }
              }
            ],
            selected: 0,
            pane: PANE.LEFT,
            currentPath: 'left'
          },
          
          [PANE.RIGHT]: {
            fileList: [
              {
                filename: 'sample.txt',
                id: 3,
                stat: {
                  mode: 0o100644,
                  size: 527
                }
              }, {
                filename: 'test.js',
                id: 4,
                stat: {
                  mode: 0o100644,
                  size: 400
                }
              }
            ],
            selected: 0,
            pane: PANE.RIGHT,
            currentPath: 'right'
          }
        }
      }
    }, this.props)));
  }
}

setup(test);

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

  t.equal(view.length, 2);
  t.end();
});

wrapper('should select either pane specified in props', (t) => {
  let view = TestUtils.renderIntoDocument(c(Wrapper));
  let main = TestUtils.findRenderedComponentWithType(view, Main);

  t.ok(main.refs.leftPane.props.current);
  t.ok(!main.refs.rightPane.props.current);
  t.end();
});

cleanup(test);
