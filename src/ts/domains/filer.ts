// define filer state type and operations.
import { FileItem } from "./file-item";
import { LocationHistory } from "./location-history";

export enum Direction {
  Up = "up",
  Down = "down",
}

export type FactoryArg = {
  id: string;
  name: string;
  location: string;
  items: FileItem[];
  currentCursorIndex: number;
  history: LocationHistory;
};

export type FilerObject = Readonly<FactoryArg>;

export type Filer = FilerObject & {
  /**
   * the node object current marked
   */
  currentFileItem: FileItem | undefined;

  /**
   * Get new instance of Filer that is moved by direction
   * @param direction direction to move index
   */
  moveIndex(direction: Direction): Filer;

  /**
   * @returns marked nodes
   */
  markedItems: FileItem[];
};

function moveIndex(this: FilerObject, direction: Direction): Filer {
  let index = 0;

  switch (direction) {
    case Direction.Down:
      index = Math.min(this.items.length - 1, this.currentCursorIndex + 1);
      break;
    case Direction.Up:
      index = Math.max(0, this.currentCursorIndex - 1);
      break;
  }

  return createFiler({ ...this, currentCursorIndex: index });
}

export const createFiler = ({ id, name, items, location, currentCursorIndex, history }: FactoryArg): Filer => {
  return {
    id,
    name,
    items,
    location,
    currentCursorIndex: Math.min(items.length - 1, Math.max(0, currentCursorIndex)),
    history,
    get currentFileItem(): FileItem | undefined {
      if (this.items.length === 0) {
        return undefined;
      }
      return this.items[this.currentCursorIndex];
    },

    /**
     * Get new instance of Filer that is moved by direction
     * @param direction direction to move index
     */
    moveIndex,

    /**
     * @returns marked nodes
     */
    get markedItems(): FileItem[] {
      const nodes = this.items.filter(v => v.marked);

      if (nodes.length === 0 && this.currentFileItem) {
        return [this.currentFileItem];
      }
      return nodes;
    },
  };
};
