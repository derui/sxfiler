import React from 'react';
import R from 'ramda';
import assert from 'power-assert';
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

/** @test {FileList} */
describe('renderer/components/FileList/FileList', () => {
  it('should be instance FileItem', () => {
    assert.ok(React.createElement(FileList) !== null);
  });

  it('should show information of files each line', () => {
    let view = TestUtils.renderIntoDocument(c(Wrapper));
    view = TestUtils.scryRenderedComponentsWithType(view, FileItem);
    let d = React.findDOMNode;

    assert.equal(view.length, 2);
  });

  it('should be top of the item when initial rendering', () => {
    let view = TestUtils.renderIntoDocument(c(Wrapper));
    view = TestUtils.scryRenderedComponentsWithType(view, FileItem);

    assert.ok(view[0].props.current);
    assert.ok(!view[1].props.current);
  });

  it('should not place cursor over backward on top of the list', () => {
    let view = TestUtils.renderIntoDocument(c(Wrapper, {
      current: 100
    }));
    let items = TestUtils.scryRenderedComponentsWithType(view, FileItem);

    assert.ok(!items[0].props.current);
    assert.ok(items[1].props.current);
  });

  it('should not place cursor over forward on bottom of the list', () => {
    let view = TestUtils.renderIntoDocument(c(Wrapper, {
      current: -100
    }));
    let items = TestUtils.scryRenderedComponentsWithType(view, FileItem);

    assert.ok(items[0].props.current);
    assert.ok(!items[1].props.current);
  });
});

