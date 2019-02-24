import * as React from "react";
import { Capability, Mode } from "../../../domains/file-stat";

// tslint:disable-next-line
const styles = require("./node-item.module.scss");

function capabilityToString(cap: Capability) {
  const readable = cap.readable ? "r" : "-";
  const writable = cap.writable ? "w" : "-";
  const executable = cap.executable ? "x" : "-";

  return `${readable}${writable}${executable}`;
}

/**
 * convert mode to string
 */
function modeToString(mode: Mode, isDirectory: boolean, isSymlink: boolean): string {
  let state = "=";

  if (isDirectory && !isSymlink) {
    state = "d";
  } else if (isSymlink) {
    state = "l";
  }

  const owner = capabilityToString(mode.owner);
  const group = capabilityToString(mode.group);
  const others = capabilityToString(mode.others);

  return `${state}${owner}${group}${others}`;
}

interface Prop {
  mode: Mode;
  isDirectory: boolean;
  isSymlink: boolean;
}

const NodeMode: React.FC<Prop> = prop => {
  const data = modeToString(prop.mode, prop.isDirectory, prop.isSymlink);

  return <span className={styles.mode}>{data}</span>;
};

export default NodeMode;
