import test from 'tape';
import React from 'react';
import ReactDOM from 'react-dom';
import R from 'ramda';
import FileList from 'sxfiler/renderer/components/file-list';
import FileItem from 'sxfiler/renderer/components/file-list/file-item';
import TestUtils from 'react-addons-test-utils';
import {PANE} from 'sxfiler/common/constants';
import {setup, cleanup} from '../jsdom-setup';

let c = React.createElement;
const defaultState = {
  state: {
    currentPane: PANE.LEFT
  },
  paneInfo: {
    fileList: [
      {
        filename: 'sample.txt',
        id: 1,
        stat: {
          mode: 0o100644,
          size: 527
        }
      }, {
        filename: 'test.js',
        id: 2,
        stat: {
          mode: 0o100644,
          size: 400
        }
      }
    ],
    currentPath: '',
    selected: 0,
    pane: PANE.LEFT
  }
};

class Wrapper extends React.Component {
  render() {
    return c('div', {}, c(FileList, R.merge(defaultState, this.props)));
  }
}

let wrapper = (comment, f) => {
  test(`renderer/components/FileList ${comment}`, f);
};

setup(test);

/** @test {FileList} */
wrapper('should be instance FileItem', (t) => {
  t.ok(React.createElement(FileList) !== null);
  t.end();
});

wrapper('should show information of files each line', (t) => {
  let view = TestUtils.renderIntoDocument(c(Wrapper));
  view = TestUtils.scryRenderedComponentsWithType(view, FileItem);

  t.equal(view.length, 2);
  t.end();
});

wrapper('should be top of the item when initial rendering', (t) => {
  let view = TestUtils.renderIntoDocument(c(Wrapper));
  view = TestUtils.scryRenderedComponentsWithType(view, FileItem);

  t.ok(view[0].props.current, 'top item selected');
  t.ok(!view[1].props.current, 'last item not selected');
  t.end();
});

wrapper('should not place cursor over backward on top of the list', (t) => {
  let props = R.clone(defaultState);
  props.paneInfo.selected = 100;

  let view = TestUtils.renderIntoDocument(c(Wrapper, props));
  let items = TestUtils.scryRenderedComponentsWithType(view, FileItem);

  t.ok(!items[0].props.current);
  t.ok(items[1].props.current);
  t.end();
});

wrapper('should not place cursor over forward on bottom of the list', (t) => {
  let props = R.clone(defaultState);
  props.paneInfo.selected = -100;
  let view = TestUtils.renderIntoDocument(c(Wrapper, props));
  let items = TestUtils.scryRenderedComponentsWithType(view, FileItem);

  t.ok(items[0].props.current);
  t.ok(!items[1].props.current);
  t.end();
});

wrapper('should show directory path on filelist', (t) => {
  let path = 'path/test/sample';
  let props = R.clone(defaultState);
  props.paneInfo.currentPath = path;
  let view = TestUtils.renderIntoDocument(c(Wrapper, props));
  let list = TestUtils.findRenderedComponentWithType(view, FileList);

  t.equal(ReactDOM.findDOMNode(list.refs.path).textContent, path);
  t.end();
});

wrapper('should mark when it is selected pane', (t) => {
  let path = 'path/test/sample';

  let props = R.clone(defaultState);
  props.paneInfo.currentPath = path;
  props.state.currentPane = PANE.LEFT;
  props.current = true;

  let view = TestUtils.renderIntoDocument(c(Wrapper, props));
  let list = TestUtils.findRenderedComponentWithType(view, FileList);

  let d = ReactDOM.findDOMNode(list.refs.path);
  t.true(d.classList.contains('is-selected'), 'contains is-selected');
  t.end();
});

cleanup(test);
