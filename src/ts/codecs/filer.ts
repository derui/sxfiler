import { Filer as Domain, createFiler } from "@/domains/filer";
import { encode as encodeFileItem } from "./file-item";
import { encode as encodeLocationHistory } from "./location-history";
import { Filer, FileList, FileItem, LocationHistory } from "@/generated/filer_pb";
import { createLocationHistory } from "@/domains/location-history";

/**
   encode filer object from RPC to frontend domain.

   @param filer JSON representation for fller
   @return Filer object
 */
export const encode = function encode(obj: Filer): Domain {
  const markedItems = obj.markedItems;
  return createFiler({
    id: obj.id,
    name: obj.name,
    location: obj.fileList?.location || "",
    items: obj.fileList?.items?.map(FileItem.create)?.map(v => encodeFileItem(v, markedItems.includes(v.id))) || [],
    currentCursorIndex: 0,
    history: obj.history
      ? encodeLocationHistory(LocationHistory.create(obj.history))
      : createLocationHistory({
          records: [],
          maxRecordNumber: 0,
        }),
  });
};
