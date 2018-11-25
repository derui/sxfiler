// define type and class of file stat

// information object of file
export interface FileStat {
  mode: string;
  uid: number;
  gid: number;
  atime: string;
  ctime: string;
  mtime: string;
  size: string;
  isDirectory: boolean;
  isFile: boolean;
  isSymlink: boolean;
}
