import classNames from "classnames";
import * as React from "react";

// tslint:disable-next-line
const styles = require("./node-item.module.scss");

interface Prop {
  isDirectory: boolean;
  isSymlink: boolean;
  name: string;
}

const NodeName: React.FC<Prop> = ({ isDirectory, name, isSymlink }) => {
  const className = classNames(styles.name, {
    [styles.nameDirectory]: isDirectory && !isSymlink,
    [styles.nameSymlink]: isSymlink,
  });

  return <span className={className}>{name}</span>;
};

export default NodeName;
