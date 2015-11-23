import test from 'tape';
import Util, {Types} from 'sxfiler/renderer/components/FileList/FileTypeUtil';

/** @test {FileTypeUtil} */
let wrapper = (label, f) => test(`renderer/components/FileList/FileTypeUtil ${label}`, f);

wrapper('should convert mode to FileType', (st) => {
  st.equal(Util.modeToFileType(0o40775), Types.DIRECTORY);
  st.equal(Util.modeToFileType(0o120775), Types.SYMLINK);
  st.equal(Util.modeToFileType(0o100775), Types.NORMAL);
  st.end();
});

wrapper('can detect file type of the mode as directory', (st) => {
  st.ok(Util.isDirectory(0o40775));
  st.ok(!Util.isDirectory(0o120775));
  st.ok(!Util.isDirectory(0o100775));
  st.end();
});

wrapper('can detect file type of the mode as symlink', (st) => {
  st.ok(!Util.isSymlink(0o40775));
  st.ok(Util.isSymlink(0o120775));
  st.ok(!Util.isSymlink(0o100775));
  st.end();
});

wrapper('can detect file type of the mode as directory', (st) => {
  st.ok(!Util.isFile(0o40775));
  st.ok(!Util.isFile(0o120775));
  st.ok(Util.isFile(0o100775));
  st.end();
});

