import test from 'tape';
import React from 'react';
import ReactDOM from 'react-dom';
import FileItem from 'sxfiler/renderer/components/file-list/file-item';
import TestUtils from 'react-addons-test-utils';
import {setup, cleanup} from '../jsdom-setup';

let c = React.createElement;

class Wrapper extends React.Component {
  render() {
    return c('table', {}, c('tbody', {}, [
      c(FileItem, this.props)
    ]));
  }
}


/** @test {FileItem} */
let wrapper = (label, f) => test(`renderer/components/FileList/FileItem ${label}`, f);

setup(test);

wrapper('should be instance FileItem', (t) => {
  t.ok(React.createElement(FileItem) !== null);
  t.end();
});

wrapper('should show file informations', (t) => {
  let view = TestUtils.renderIntoDocument(c(Wrapper, {
    item: {
      filename: 'sample.txt',
      stat: {
        mode: 0o100644,
        size: 527
      }
    }
  }));
  view = TestUtils.findRenderedComponentWithType(view, FileItem);
  let d = ReactDOM.findDOMNode;

  t.equal(d(view.refs.filename).innerHTML, 'sample.txt');
  t.equal(d(view.refs.mode).innerHTML, '-rw-r--r--');
  t.ok(d(view).classList.contains('file-list__item--normal'));
  t.equal(d(view.refs.size).innerHTML, '527');
  t.end();
});

wrapper('should mark as current when setted current prop', (t) => {
  let view = TestUtils.renderIntoDocument(c(Wrapper, {
    current: true
  }));
  view = TestUtils.findRenderedComponentWithType(view, FileItem);
  let d = ReactDOM.findDOMNode;

  t.ok(d(view).classList.contains('is-selected'));
  t.end();
});

cleanup(test);
