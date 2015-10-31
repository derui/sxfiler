import test from 'tape';
import Util from 'sxfiler/main/FileListUtil';
import tmp from 'tmp';
import fs from 'fs';
import path from 'path';

let obj = {};
let before = () => {
  obj = tmp.dirSync();

  fs.writeFileSync(path.join(obj.name, 'file.txt'), 'data', {
    mode: 0o644
  });
};

let after = () => {
  fs.unlinkSync(path.join(obj.name, 'file.txt'));
  obj.removeCallback();
};

test('FileListUtil#getFileInformationsOfDirectory', (t) => {
  t.comment('can get file lists from given path');
  before();

  Util.getFileInformationsOfDirectory(fs, obj.name).done((files) => {
    t.equal(Object.keys(files).length, 1);
    t.equal(files['file.txt'].mode & 0o777, 0o644);
    after();
    t.end();
  });
});
