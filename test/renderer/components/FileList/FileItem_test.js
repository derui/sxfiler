import React from 'react';
import assert from 'power-assert';
import FileItem from 'sxfiler/renderer/components/FileList/FileItem';
import TestUtils from 'react-addons-test-utils';

let c = React.createElement;

class Wrapper extends React.Component {
  render() {
    return c('table', {}, c('tbody', {}, [
        c(FileItem, this.props)
    ]));
  }
}

/** @test {FileItem} */
describe('renderer/components/FileList/FileItem', () => {
  it('should be instance FileItem', () => {
    assert.ok(React.createElement(FileItem) !== null);
  });

  it('should show file informations', () => {
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
    let d = React.findDOMNode;

    assert.equal(d(view.refs.filename).innerHTML, 'sample.txt');
    assert.equal(d(view.refs.mode).innerHTML, '-rw-r--r--');
    assert.ok(d(view.refs.mode).classList.contains('file-list__item__mode--normal'));
    assert.equal(d(view.refs.size).innerHTML, '527');
  });

  it('should mark as current when setted current prop', () => {
    let view = TestUtils.renderIntoDocument(c(Wrapper, {
      current: true
    }));
    view = TestUtils.findRenderedComponentWithType(view, FileItem);
    let d = React.findDOMNode;

    assert.ok(d(view).classList.contains('is-selected'));
  });
});

