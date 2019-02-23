import * as React from "react";
import { Capability, Mode } from "../../domains/file-stat";

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

  if (isDirectory) {
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

const FileItemMode: React.FC<Prop> = prop => {
  const data = modeToString(prop.mode, prop.isDirectory, prop.isSymlink);

  return <span className="fp-FileItem_FileMode">{data}</span>;
};

export default FileItemMode;
