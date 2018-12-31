import * as React from "react";
import classNames from "classnames";

interface Prop {
  className: string;
  isDirectory: boolean;
  isSymlink: boolean;
  name: string;
}

const FileItemName : React.SFC<Prop> = prop => {
  const className = classNames(
    prop.className, {
      ["${prop.className}-directory"]: prop.isDirectory,
      ["${prop.className}-symlink"]: prop.isSymlink,
    });

  return (
    <span className={className}>{prop.name}</span>
  )
}

export default FileItemName;
