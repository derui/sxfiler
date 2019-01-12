import * as React from "react";
import classNames from "classnames";

interface Prop {
  isDirectory: boolean;
  isSymlink: boolean;
  name: string;
}

const baseName = "fp-FileItem_FileName";

const FileItemName : React.FC<Prop> = ({isDirectory, name, isSymlink}) => {
    const className = classNames(baseName, {
      ["${baseName}-directory"]: isDirectory,
      ["${baseName}-symlink"]: isSymlink,
    });

  return (
    <span className={className}>{name}</span>
  )
}

export default FileItemName;
