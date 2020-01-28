import { h } from "preact";
import { Capability, Mode } from "@/generated/filer_pb";

const capabilityToString = function capabilityToString(cap: Capability.AsObject | undefined) {
  if (!cap) {
    return "---";
  }

  const readable = cap.readable ? "r" : "-";
  const writable = cap.writable ? "w" : "-";
  const executable = cap.executable ? "x" : "-";

  return `${readable}${writable}${executable}`;
};

/**
 * convert mode to string
 */
const modeToString = function modeToString(mode: Mode.AsObject, isDirectory: boolean, isSymlink: boolean): string {
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
};

interface Prop {
  mode?: Mode.AsObject;
  isDirectory: boolean;
  isSymlink: boolean;
}

export const Component: preact.FunctionComponent<Prop> = ({ mode, isDirectory, isSymlink }) => {
  if (!mode) {
    return null;
  }

  const data = modeToString(mode, isDirectory, isSymlink);

  return (
    <span class="file-item__item-mode" data-testid="fileItem-modeSlot">
      {data}
    </span>
  );
};
