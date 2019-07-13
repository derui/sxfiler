import * as React from "react";
import { styled } from "@/components/theme";
import { CapabilityObject } from "@/domains/capability";
import { ModeObject } from "@/domains/mode";

function capabilityToString(cap: CapabilityObject) {
  const readable = cap.readable ? "r" : "-";
  const writable = cap.writable ? "w" : "-";
  const executable = cap.executable ? "x" : "-";

  return `${readable}${writable}${executable}`;
}

/**
 * convert mode to string
 */
function modeToString(mode: ModeObject, isDirectory: boolean, isSymlink: boolean): string {
  let state = "-";

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
  mode: ModeObject;
  isDirectory: boolean;
  isSymlink: boolean;
}

const Mode = styled.span`
  flex: 0 1 auto;
  padding: 0 ${props => props.theme.spaces.small};

  white-space: nowrap;
`;

export const Component: React.FC<Prop> = prop => {
  const data = modeToString(prop.mode, prop.isDirectory, prop.isSymlink);

  return <Mode>{data}</Mode>;
};
