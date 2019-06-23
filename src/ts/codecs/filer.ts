import { Filer, createFiler } from "../domains/filer";
import { FileItemOnRPC, encode as encodeFileItem } from "./file-item";

// define codec that is between filer domain and RPC

export type FilerOnRPC = {
  id: string;
  name: string;
  fileList: {
    location: string;
    items: FileItemOnRPC[];
  };
  markedItems: string[];
  sortOrder: string;
};

/**
   encode filer object from RPC to frontend domain.

   @param filer JSON representation for fller
   @return Filer object
 */
export const encode = (obj: FilerOnRPC): Filer => {
  const {
    id,
    name,
    fileList: { location, items },
    markedItems,
  } = obj;

  return createFiler({
    id,
    name,
    location,
    items: items
      .map(v => ({
        ...v,
        marked: markedItems.includes(v.id),
      }))
      .map(encodeFileItem),
    currentCursorIndex: 0,
  });
};
