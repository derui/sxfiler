import test from 'tape';
import React from 'react';
import R from 'ramda';
import FileList from 'sxfiler/renderer/components/FileList';
import FileItem from 'sxfiler/renderer/components/FileList/FileItem';
import TestUtils from 'react-addons-test-utils';

let c = React.createElement;

class Wrapper extends React.Component {
  render() {
    return c('div', {}, c(FileList, R.merge({
      files: {
        'sample.txt' : {
          mode: 0o100644,
          size: 527
        },
        'test.js': {
          mode: 0o100644,
          size: 400
        }
      }
    }, this.props)));
  }
}

let wrapper = (comment, f) => {
  test(`renderer/components/FileList ${comment}`, f);
};

/** @test {FileList} */
wrapper('should be instance FileItem', (t) => {
  t.ok(React.createElement(FileList) !== null);
  t.end();
});

wrapper('should show information of files each line', (t) => {
  let view = TestUtils.renderIntoDocument(c(Wrapper));
  view = TestUtils.scryRenderedComponentsWithType(view, FileItem);
  let d = React.findDOMNode;

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
  let view = TestUtils.renderIntoDocument(c(Wrapper, {
    paneInfo: {
      position: 100
    }
  }));
  let items = TestUtils.scryRenderedComponentsWithType(view, FileItem);

  t.ok(!items[0].props.current);
  t.ok(items[1].props.current);
  t.end();
});

wrapper('should not place cursor over forward on bottom of the list', (t) => {
  let view = TestUtils.renderIntoDocument(c(Wrapper, {
    current: -100
  }));
  let items = TestUtils.scryRenderedComponentsWithType(view, FileItem);

  t.ok(items[0].props.current);
  t.ok(!items[1].props.current);
  t.end();
});

wrapper('should be show directory path on filelist', (t) => {
  let path = 'path/test/sample';
  let view = TestUtils.renderIntoDocument(c(Wrapper, {
    path
  }));
  let list = TestUtils.findRenderedComponentWithType(view, FileList);

  t.equal(React.findDOMNode(list.refs.path).textContent, path);
  t.end();
});
