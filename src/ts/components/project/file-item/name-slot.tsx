import { h } from "preact";

interface Prop {
  isDirectory: boolean;
  isSymlink: boolean;
  name: string;
}

export const Component: preact.FunctionComponent<Prop> = ({ isDirectory, name, isSymlink }) => {
  return (
    <span
      class="file-item__item-name"
      data-testid="fileItem-nameSlot"
      data-directory={isDirectory && !isSymlink}
      data-symlink={isSymlink}
    >
      {name}
    </span>
  );
};
