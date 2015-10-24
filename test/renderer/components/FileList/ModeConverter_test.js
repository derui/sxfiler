import assert from 'power-assert';
import func from 'sxfiler/renderer/components/FileList/ModeConverter';

/** @test {ModeConverter} */
describe('renderer/components/FileList/ModeConverter', () => {
  it('should return ---------- when given NaN', () => {
    assert.equal(func(NaN), '----------');
  });

  it('should return ---------- when type of given value is not number', () => {
    assert.equal(func('0'), '----------');
    assert.equal(func(''), '----------');
    assert.equal(func([]), '----------');
    assert.equal(func({}), '----------');
    assert.equal(func(null), '----------');
    assert.equal(func(undefined), '----------');
  });

  it('should return posix-style permission of given permission', () => {
    [0, 1, 2].forEach((shift) => {
      let sliceBase = 10 - (3 * (shift + 1));

      assert.equal(func(0o000 << (shift * 3)).slice(sliceBase, sliceBase + 3), '---');
      assert.equal(func(0o001 << (shift * 3)).slice(sliceBase, sliceBase + 3), '--x');
      assert.equal(func(0o002 << (shift * 3)).slice(sliceBase, sliceBase + 3), '-w-');
      assert.equal(func(0o003 << (shift * 3)).slice(sliceBase, sliceBase + 3), '-wx');
      assert.equal(func(0o004 << (shift * 3)).slice(sliceBase, sliceBase + 3), 'r--');
      assert.equal(func(0o005 << (shift * 3)).slice(sliceBase, sliceBase + 3), 'r-x');
      assert.equal(func(0o006 << (shift * 3)).slice(sliceBase, sliceBase + 3), 'rw-');
      assert.equal(func(0o007 << (shift * 3)).slice(sliceBase, sliceBase + 3), 'rwx');
    });
  });

  it('should return file type of directory or symlink', () => {
    assert.equal(func(0o40775), 'drwxrwxr-x');
    assert.equal(func(0o120777), 'lrwxrwxrwx');
    assert.equal(func(0o100777), '-rwxrwxrwx');
  });
});
