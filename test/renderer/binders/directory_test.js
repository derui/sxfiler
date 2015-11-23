import test from 'tape';
import * as D from 'sxfiler/renderer/actions/directory';
import Binder from 'sxfiler/renderer/binders/directory';
import Rx from 'rx';
import {PANE} from 'sxfiler/common/constants';

/**
 * @test {Directory~binders}
 */
let wrapper = (comment, f) => test(`renderer/binders/Directory ${comment}`, f);

wrapper('should be able to bind directory actions', (t) => {
  let stream = new Rx.Subject();
  let binder = new Binder(PANE.LEFT, stream);

  // first value of store stream is default value... 
  binder.store.getObservable().slice(1).subscribe((v) => {
    t.deepEqual(v, {pane:PANE.LEFT, currentPath: 'foo', fileList: [], selected: 0});
    t.end();
  });

  stream.onNext({key: D.ACTIONS.REQUEST_DIRECTORY, path: 'foo', pane: PANE.LEFT});
});

wrapper('should handle event when finish file list traversing', (t) => {
  let stream = new Rx.Subject();
  let binder = new Binder(PANE.LEFT, stream);

  // first value of store stream is default value... 
  binder.store.getObservable().slice(1).subscribe((v) => {
    t.deepEqual(v, {currentPath: 'foo', fileList: [
      {filename: 'file1'}, {filename: 'file2'}
    ], selected:0, pane: PANE.LEFT});
    t.end();
  });

  stream.onNext({key: D.ACTIONS.RECEIVE_DIRECTORY, path: 'foo', fileList: [
    {filename: 'file1'}, {filename: 'file2'}
  ], pane: PANE.LEFT});
});
