import test from 'tape';
import func from 'sxfiler/renderer/components/FileList/ModeConverter';

/** @test {ModeConverter} */
test('renderer/components/FileList/ModeConverter', (t) => {
  t.test('should return ---------- when given NaN', (t) => {
    t.equal(func(NaN), '----------');
    t.end();
  });

  t.test('should return ---------- when type of given value is not number', (t) => {
    t.equal(func('0'), '----------');
    t.equal(func(''), '----------');
    t.equal(func([]), '----------');
    t.equal(func({}), '----------');
    t.equal(func(null), '----------');
    t.equal(func(undefined), '----------');
    t.end();
  });

  t.test('should return posix-style permission of given permission', (t) => {
    [0, 1, 2].forEach((shift) => {
      let sliceBase = 10 - (3 * (shift + 1));

      t.equal(func(0o000 << (shift * 3)).slice(sliceBase, sliceBase + 3), '---');
      t.equal(func(0o001 << (shift * 3)).slice(sliceBase, sliceBase + 3), '--x');
      t.equal(func(0o002 << (shift * 3)).slice(sliceBase, sliceBase + 3), '-w-');
      t.equal(func(0o003 << (shift * 3)).slice(sliceBase, sliceBase + 3), '-wx');
      t.equal(func(0o004 << (shift * 3)).slice(sliceBase, sliceBase + 3), 'r--');
      t.equal(func(0o005 << (shift * 3)).slice(sliceBase, sliceBase + 3), 'r-x');
      t.equal(func(0o006 << (shift * 3)).slice(sliceBase, sliceBase + 3), 'rw-');
      t.equal(func(0o007 << (shift * 3)).slice(sliceBase, sliceBase + 3), 'rwx');
    });
    t.end();
  });

  t.test('should return file type of directory or symlink', (t) => {
    t.equal(func(0o40775), 'drwxrwxr-x');
    t.equal(func(0o120777), 'lrwxrwxrwx');
    t.equal(func(0o100777), '-rwxrwxrwx');
    t.end();
  });

  t.end();
});
