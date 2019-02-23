/**
 * This component provides dummy element to be able to measure size of item in list anytime.
 */
import classNames from "classnames";
import * as React from "react";

import { emptyMode } from "../../domains/file-stat";
import FileMode from "./file-item-mode";
import FileName from "./file-item-name";
import FileSize from "./file-item-size";
import FileTimestamp from "./file-item-timestamp";

const DummyItem: React.FC = () => {
  const className = classNames("fp-FileItem", "fp-FileItem-dummy");

  return (
    <li className={className}>
      <FileMode key="mode" mode={emptyMode()} isDirectory={false} isSymlink={false} />
      <FileTimestamp key="timestamp" timestamp={new Date(0)} />
      <FileSize key="size" size="0" />
      <FileName key="name" name="dummy" isDirectory={false} isSymlink={false} />
    </li>
  );
};

export default DummyItem;
