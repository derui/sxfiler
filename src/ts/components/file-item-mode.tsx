import * as React from "react";
import {Capability, FileStat} from "../domain/file-stat";

function capabilityToString(cap: Capability) {
  const readable = cap.readable ? "r" : "-";
  const writable = cap.writable ? "w" : "-";
  const executable = cap.executable ? "x" : "-";

  return `${readable}${writable}${executable}`;
}

/**
 * convert mode to string
 */
function modeToString(stat: FileStat) : string {
  var state = "=";

  if (stat.isDirectory) {
    state = "d";
  } else if (stat.isSymlink) {
    state = "l";
  }

  const owner = capabilityToString(stat.mode.owner);
  const group = capabilityToString(stat.mode.group);
  const other = capabilityToString(stat.mode.other);

  return `${state}${owner}${group}${other}`;
}

interface Prop {
  stat: FileStat;
  className: string;
}

const FileItemMode : React.SFC<Prop> = prop => {
  const data = modeToString(prop.stat);

  return (<span className={prop.className}>{data}</span>);
};

export default FileItemMode;
