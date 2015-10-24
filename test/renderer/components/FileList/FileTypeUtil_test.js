import assert from 'power-assert';
import Util, {Types} from 'sxfiler/renderer/components/FileList/FileTypeUtil';

/** @test {FileTypeUtil} */
describe('renderer/components/FileList/FileTypeUtil', () => {
  it('should convert mode to FileType', () => {
    assert.equal(Util.modeToFileType(0o40775), Types.DIRECTORY);
    assert.equal(Util.modeToFileType(0o120775), Types.SYMLINK);
    assert.equal(Util.modeToFileType(0o100775), Types.NORMAL);
  });

  it('can detect file type of the mode as directory', () => {
    assert.ok(Util.isDirectory(0o40775));
    assert.ok(!Util.isDirectory(0o120775));
    assert.ok(!Util.isDirectory(0o100775));
  });

  it('can detect file type of the mode as symlink', () => {
    assert.ok(!Util.isSymlink(0o40775));
    assert.ok(Util.isSymlink(0o120775));
    assert.ok(!Util.isSymlink(0o100775));
  });

  it('can detect file type of the mode as directory', () => {
    assert.ok(!Util.isFile(0o40775));
    assert.ok(!Util.isFile(0o120775));
    assert.ok(Util.isFile(0o100775));
  });
});

